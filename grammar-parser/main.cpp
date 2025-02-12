#include <algorithm>        // find elements within containers
#include <iostream>         // interact with cout & cerr
#include <fstream>          // read file
#include <map>              // tokens require sorting for making the output deterministic
#include <queue>            // algorithm for performing breadth-first traversal
#include <set>              // tokens require sorting for making the output deterministic
#include <string>           // yes
#include <sstream>          // for process multiple bits simultaneously, and then print to file sequentially
#include <unordered_map>    // faster accesses for mapping input to output
#include <unordered_set>    // faster accesses for set operations, and usage as map keys
#include <utility>          // pair
#include <vector>           // storing elements with order

using namespace std;

struct parser_t {
    // Define tokens, printable as the enum ParserToken*, with a associated type Parser*
    struct token_t {
        virtual ~token_t() {}

        const token_t* id() const noexcept { return this; }
        virtual string token_raw() const noexcept =0;
        virtual string token_name() const noexcept =0;
        virtual string token_type() const noexcept =0;
    };
    struct char_token_t: public token_t {
        string t_name, t_print;
        char_token_t(int id, const string& raw): token_t() {
            t_name = "Token" + to_string(id);
            t_print = char_token_t::escape_quotes(raw);
        }
        static string escape_quotes(const string& raw) {
            stringstream ss;
            for (auto c : raw) {
                if (c == '"' || c == '\\') ss << '\\';
                ss << c;
            }
            return ss.str(); 
        }
        virtual string token_raw() const noexcept { return this->t_print; }
        virtual string token_name() const noexcept { return this->t_name; }
        virtual string token_type() const noexcept { return "String"; }
    };
    struct grammar_token_t: public token_t {
        string raw;
        grammar_token_t(const string& raw): token_t(),raw(raw) {}
        virtual string token_raw() const noexcept { return this->raw; }
        virtual string token_name() const noexcept { return "Token"+this->raw; }
        virtual string token_type() const noexcept { return "Parser"+this->raw; }
    };
    // Minimal regex parser definer, so that the matching logic can be written in Elm
    struct token_parser_t {
        struct node_t {
            struct set_hash {
                size_t operator() (const unordered_set<const node_t*>& s) const {
                    hash<void*> hasher;
                    size_t result{0};
                    for (const node_t* ptr : s) result ^= hasher((void*)ptr);
                    return result;
                }
            };
            struct pair_hash {
                size_t operator() (const pair<char,char>& p) const {
                    hash<char> hasher;
                    // Shift it so that the ordering makes a difference;
                    return hasher(p.first) ^ (hasher(p.second) << 1);
                }
            };
            node_t *next_node;  // from specifying specific characters
            set<pair<char,char>> next; // character set for going to next
            bool allow_skip;    // from specifying *, this will include next_node->next_state(..) in the calculations
            bool allow_others; // from specifying \o
            bool allow_repeat; // from specifying + or *
            const char_token_t* end;
            node_t(): next_node(NULL), next(), allow_skip(false), allow_others(false), allow_repeat(false), end(NULL) {}
            node_t(node_t& t) =delete;
            node_t(const node_t& t) =delete;
            ~node_t() { if (next_node != NULL) delete next_node; }
            bool token_in_set(pair<char,char> token) const {
                if (token == pair<char,char>('\0', '\0')) return allow_others;
                for (auto [low, high]: next) {
                    if (low >= token.first && low <= token.second) {
                        if (high > token.second) throw "token is out of range";
                        return true;
                    } else if (high >= token.first && high <= token.second) {
                        throw "token is out of range";
                    }
                }
                return false;
            }
            static const char_token_t* end_token(const unordered_set<const node_t*>& s) {
                unordered_set<const char_token_t*> t;
                for (auto ptr: s) {
                    // Go as far as possible
                    while (ptr->next_node != NULL && ptr->next_node->allow_skip) ptr = ptr->next_node;
                    if (ptr->end != NULL) t.insert(ptr->end);
                }
                if (t.empty()) return NULL;
                if (t.size() == 1) return *t.begin();
                string error_message = "multiple tokens result in the same end state: ";
                for (auto ptr: t) error_message += ptr->token_raw() + " ";
                throw error_message;
            }
            static unordered_set<const node_t*> step(const unordered_set<const node_t*>& s, pair<char,char> token) {
                unordered_set<const node_t*> result;
                for (auto ptr: s) {
                    if (ptr->allow_repeat && ptr->token_in_set(token)) result.insert(ptr);
                    if (ptr->next_node == NULL) continue;
                    // Check for optionals
                    while (ptr->next_node != NULL) {
                        ptr = ptr->next_node;
                        if (ptr->token_in_set(token)) result.insert(ptr);
                        if (!ptr->allow_skip) break;
                    }
                }
                return result;
            }
        };

        unordered_set<const node_t*> roots;
        set<pair<char,char>> all_tokens;

        token_parser_t():roots(), all_tokens() {}
        token_parser_t(token_parser_t& o) =delete;
        token_parser_t(const token_parser_t& o) =delete;
        token_parser_t(token_parser_t&& o) noexcept: roots(std::move(o.roots)), all_tokens(std::move(o.all_tokens)) {}
        ~token_parser_t() { for (const node_t* root : roots) delete root; }
        void add_token(const char_token_t* id, const string& input) {
            string::const_iterator begin(input.cbegin()), end(input.cend());
            if (*begin != '"') throw "Expecting token to begin with '\"' in: " + input;
            ++begin;
            char last_seen('\0');
            node_t* initial_node = new node_t(), *prev_node=initial_node, *current_node = prev_node->next_node = new node_t();
            this->roots.insert(initial_node);
            while (begin != end) {
                switch (*begin) {
                    case '[': { // start multirange
                        char prev('\0');
                        ++begin;
                        while (begin != end && *begin != ']') {
                            switch (*begin) {
                            case '\\': // start escape
                                if (prev != '\0') { current_node->next.insert({prev, prev}); all_tokens.insert({prev,prev}); }
                                ++begin;
                                if (begin == end) throw "Unexpected end of the token definition in: " + input;
                                if (*begin == 'o') current_node->allow_others = true;
                                else prev = *begin;
                                ++begin;
                                break;
                            case '-': // possibly start a range
                                if (prev == '\0') { prev = '-'; continue; }
                                ++begin;
                                if (begin == end) throw "Unexpected end of the token definition in: " + input;
                                switch (*begin) {
                                    case ']': // end of group
                                        current_node->next.insert({prev,prev}); all_tokens.insert({prev,prev});
                                        current_node->next.insert({'-','-'}); all_tokens.insert({'-','-'});
                                        break;
                                    default: { // any other char
                                        current_node->next.insert({prev,*begin}); all_tokens.insert({prev, *begin});
                                        prev = '\0';
                                        ++begin;
                                    }
                                }
                                break;
                            default: // anything else
                                if (prev != '\0') { current_node->next.insert({prev,prev}); all_tokens.insert({prev,prev}); }
                                prev = *begin;
                                ++begin;
                            }
                        }
                        if (begin == end) throw "Unexpected end of token definition in: " + input;
                        prev_node = current_node; current_node = prev_node->next_node = new node_t();
                        ++begin;
                        }
                        break;
                    case '\\': // start escape
                        ++begin;
                        if (begin == end) throw "Unexpected end of token definition in: " + input;
                        if (*begin == 'o') current_node->allow_others = true;
                        else { current_node->next.insert({*begin,*begin}); all_tokens.insert({*begin,*begin}); }
                        prev_node = current_node; current_node = prev_node->next_node = new node_t();
                        ++begin;
                        break;
                    case '+': // repeat itself as 
                        if (prev_node == initial_node) throw "Unable to use '+' without any prefix in: " + input;
                        prev_node->allow_repeat = true;
                        ++begin;
                        break;
                    case '*': // repeat itself as 
                        if (prev_node == initial_node) throw "Unable to use '*' without any prefix in: " + input;
                        prev_node->allow_repeat = true;
                        prev_node->allow_skip = true;
                        ++begin;
                        break;
                    case '?': // repeat itself as 
                        if (prev_node == initial_node) throw "Unable to use '?' without any prefix in: " + input;
                        prev_node->allow_skip = true;
                        ++begin;
                        break;
                    case '"': // terminal
                        if (++begin != end) throw "Expected end after '\"' in: " + input;
                        break;
                    default:
                        current_node->next.insert({*begin,*begin}); all_tokens.insert({*begin,*begin});
                        prev_node = current_node; current_node = prev_node->next_node = new node_t();
                        ++begin;
                }
            }
            prev_node->end = id;
            prev_node->next_node = NULL;
            delete current_node;
        }
        // Printing related
        set<pair<char,char>> unique_char_ranges() const {
            if (all_tokens.empty()) return {};
            set<pair<char,char>> confirmed;
            char prev_low;
            set<char> upper_bounds;
            // This algorithm expects the iteration of all_tokens to be ordered
            for (auto [low, high] : all_tokens) {
                // no lingering range from before
                if (upper_bounds.empty()) { prev_low = low; upper_bounds.insert(high); continue; }
                // lingering range from before
                auto begin = upper_bounds.cbegin(), end = upper_bounds.cend();
                // Process all ranges that can be cleared
                while (begin != end && low > *begin) { confirmed.insert({prev_low, *begin}); prev_low = *begin+1; ++begin; }
                // no more ranges to clear
                if (begin == end) { prev_low = low; upper_bounds.clear(); upper_bounds.insert(high); }
                // insert regions into upper_bounds
                else {
                    upper_bounds.erase(upper_bounds.cbegin(), begin);
                    upper_bounds.insert(high);
                    // stop before the lower-bound, since that's the start of the next token
                    if (prev_low != low) upper_bounds.insert(low-1);
                }
            }
            for (char next: upper_bounds) { confirmed.insert({prev_low, next}); prev_low = next+1; }
            return confirmed;
        }
        static void print_char_set(ostream& stream, const set<pair<char,char>>& all_chars) {
            stream << '"';
            for (auto tok: all_chars) for (char i = tok.first; i <= tok.second; i++) { if (i == '"' || i == '\\') stream << '\\'; stream << i; }
            stream << '"';
        }
        static void print_state_transition(ostream& stream, int to, const char_token_t* end_token) {
            if (to != -1) stream << "({state | seen = state.seen ++ char, state = " << to << "}, Nothing)" << endl;
            else {
                stream << "let (newState, _) = parserProcessChar {seen = \"\", state=0} char in (newState, ";
                if (end_token == NULL) stream << "Nothing)" << endl ;
                else stream << "Just (Parser" << end_token->token_name() << " state.seen))" << endl;
            }
        }
        static int state_number(unordered_map<unordered_set<const node_t*>, int, node_t::set_hash>& seen, queue<unordered_set<const node_t*>>& to_process, const unordered_set<const node_t*>& state) {
            if (state.empty()) return -1;
            auto loc = seen.find(state);
            if (loc != seen.cend()) return loc->second;
            int next_num = seen.size();
            seen[state] = next_num;
            to_process.push(state);
            return next_num;
        }
        void print(ostream& stream) const {
            const auto chars = unique_char_ranges();
            int count = 0;
            stream << R"""(-- TokenState
type alias ParserStateToken =
    {   seen: String
    ,   state: Int
    }

parserProcessChar: ParserStateToken -> String -> (ParserStateToken, Maybe ParserToken)
parserProcessChar state char = case state.state of)""" << endl;

            stringstream end_stream;
            int state(0);
            unordered_map<unordered_set<const node_t*>, int, node_t::set_hash> seen;
            queue<unordered_set<const node_t*>> to_process;
            seen[roots] = 0;
            to_process.push(roots);
            while (!to_process.empty() && state < 20) {
                auto current = to_process.front(); to_process.pop();
                stream << "    " << state++ << " -> ";
                // Check for end token & others
                auto end_token = node_t::end_token(current);
                auto other_index = state_number(seen, to_process, node_t::step(current, {'\0','\0'}));
                map<int,set<pair<char,char>>> transitions;
                // Add transitions
                for (pair<char,char> char_set : chars) {
                    auto next_index = state_number(seen, to_process, node_t::step(current, char_set));
                    if (next_index != other_index) transitions[next_index].insert(char_set);
                }
                for (auto [index, char_set]: transitions) {
                    stream << "if String.contains char ";
                    token_parser_t::print_char_set(stream, char_set);
                    stream << "\n        then ";
                    token_parser_t::print_state_transition(stream, index, end_token);
                    stream << "        else ";
                }
                token_parser_t::print_state_transition(stream, other_index, end_token);
                // Add terminal case
                if (end_token != NULL) end_stream << "    " << (state-1) << " -> Just (Parser" << end_token->token_name() << " state.seen)" << endl;
            }
            stream << R"""(    _ -> ({seen = "", state = 0}, Nothing)

parserProcessEnd: ParserStateToken -> Maybe ParserToken
parserProcessEnd state = case state.state of)""" << endl;
            stream << end_stream.str();
            stream << "    _ -> Nothing" << endl << endl;
        }
    };
    struct grammar_parser_t {
        struct node_t {
            struct set_hash {
                size_t operator() (const unordered_set<const node_t*>& s) const {
                    hash<void*> hasher;
                    size_t result{0};
                    for (auto ptr : s) result ^= hasher((void*)ptr);
                    return result;
                }
            };
            const token_t* token;
            int rule_no, input_no;
            node_t *next;
            node_t(const token_t* t, int rule_no, int input_no):token(t),next(NULL),rule_no(rule_no),input_no(input_no){}
            ~node_t() { if (next !=NULL) delete next; }
        };
        struct transition_state_t {
            unordered_set<const node_t*> nodes;
            int end_no, rule_no, input_no;

            transition_state_t(unordered_set<const node_t*>&& nodes, int end_no, int rule_no, int input_no): nodes(nodes),end_no(end_no),rule_no(rule_no),input_no(input_no) {}
            void print_case_start(ostream& stream, int state_no) {
                stream << "    (" << state_no << "::" << (end_no == -1 ? "_" : "states") << ") -> case token of" << endl;;
            }
            void print_case_close(ostream& stream) {
                if (end_no == -1) stream << "        other -> Result.Err (parserError" << rule_no << " " << input_no << " other)" << endl;
                else stream << "        other -> parserProcessRule" << end_no << " {state | states=states} |> Result.andThen (\\s -> parserProcessToken other s)" << endl;
            }
            transition_state_t step(const unordered_map<const token_t*,pair<unordered_set<const node_t*>,bool>>& dict, const token_t* t) {
                unordered_set<const node_t*> next;
                for (auto n : nodes) if (n->token == t) next.insert(n->next);
                return transition_state_t::expand(dict, next);
            }
            static transition_state_t expand(const unordered_map<const token_t*,pair<unordered_set<const node_t*>,bool>>& dict, unordered_set<const node_t*> set) {
                queue<const node_t*> to_visit;
                int end_no(-1), rule_no, input_no;
                for (auto ptr : set) to_visit.push(ptr);
                while (!to_visit.empty()) {
                    const node_t* next = to_visit.front(); to_visit.pop();
                    // Find the earliest rule + latest token
                    if (rule_no == 0 || next->rule_no < rule_no) {
                        rule_no = next->rule_no;
                        input_no = next->input_no;
                    }
                    // Find the end token (ideally, each state should only have one)
                    if (next->token == NULL) {
                        if (end_no != -1) throw "Possible conflict between Rule " + to_string(end_no) + " and Rule " + to_string(next->rule_no);
                        end_no = next->rule_no;
                        continue;
                    }
                    // Find new rules with epsilon in the next token
                    auto rule = dict.find(next->token);
                    if (rule == dict.end()) continue; // a string token
                    if (rule->second.second && set.find(next->next) == set.cend()) { set.insert(next->next); to_visit.push(next->next); }
                    for (auto ptr: rule->second.first) {
                        if (set.find(ptr) == set.cend()) { set.insert(ptr); to_visit.push(ptr); }
                    }
                }
                return transition_state_t(std::move(set), end_no, rule_no, input_no);
            }
        };

        const token_t* main_token;
        unordered_map<const token_t*,pair<unordered_set<const node_t*>,bool>> head;
        unordered_map<const token_t*,unordered_set<const token_t*>> first;
        vector<pair<int,pair<const token_t*,vector<const token_t*>>>> raw_rules;

        grammar_parser_t(): main_token(NULL), head(), first(), raw_rules() {}
        grammar_parser_t(grammar_parser_t& o) =delete;
        grammar_parser_t(const grammar_parser_t& o) =delete;
        grammar_parser_t(grammar_parser_t&& o): main_token(o.main_token), head(std::move(o.head)), first(o.first), raw_rules(o.raw_rules) {}
        ~grammar_parser_t() { for (auto [_, all_start]: head) for (auto ptr: all_start.first) delete ptr; }

        void add_rules(int line_no, const token_t* return_token, const vector<const token_t*>& tokens) {
            if (main_token == NULL) {
                // Add the default rule
                main_token = return_token;
                raw_rules.push_back({0,{return_token,{return_token}}});
            }
            if (tokens.empty()) { head[return_token].second = true; return; }
            raw_rules.push_back({line_no, {return_token, tokens}});
            node_t *ptr, **ptr_ptr = &ptr;
            int input_no = 1;
            for (const token_t* t: tokens) { *ptr_ptr = new node_t(t, line_no, input_no++); ptr_ptr = &((*ptr_ptr)->next); }
            *ptr_ptr = new node_t(NULL, line_no, 0); // To specify the return
            head[return_token].first.insert(ptr);
            first[return_token].insert(tokens[0]);
        }
        void print(ostream& stream, const vector<const token_t*>& token_order) const {
            if (main_token == NULL) throw "No rules are registered";
            stream << R"""(-- Transitions
parserParse: String -> Result String ParserExpression
parserParse input =
    input
    |>  String.foldl
        (\char result -> case result of
            Result.Err _ -> result
            Result.Ok (tokenState, parseState) ->
                String.fromChar char
                |> parserProcessChar tokenState
                |> (\(newState, token) ->
                    case token of
                        Nothing -> Result.Ok (newState, parseState)
                        Just t -> parserProcessToken t parseState
                            |> Result.map (\pState -> (newState, pState))
                )
        )
        (Result.Ok ({seen="", state=0}, {stack=[], states=[0]}))
    |>  Result.andThen (\(tokenState, parseState) ->
            parserProcessEnd tokenState
            |> Maybe.map (\tok -> parserProcessToken tok parseState )
            |> Maybe.withDefault (Result.Ok parseState)
        )
    |>  Result.andThen (\pState ->
        pState
        |> parserProcessToken ParserTokenEnd
        |> Result.andThen (\state -> case state.stack of
            [e] -> parserExpectTokenExpression e |> Result.mapError (\_ -> "Parser result has incorrect type")
            [] -> Result.Err "Parser result is missing"
            _ -> Result.Err "Parser did not complete"
        )
    )

type alias ParserState =
    {   stack: List ParserToken
    ,   states: List Int
    }

parserExtractLastN: Int -> (ParserState, List ParserToken) -> Result () (ParserState, List ParserToken)
parserExtractLastN remaining (state, stack) = case remaining of
    0 -> Result.Ok (state, stack)
    r -> case parserExtractLastN ( r - 1 ) (state,stack) of
        Result.Ok (newState, newStack) -> case newState.stack of
            (e::others) -> Result.Ok ({newState | stack = others}, (e::newStack))
            [] -> Result.Err ()
        Result.Err () -> Result.Err ()

parserExtract: (ParserToken -> Result () token) -> (Int -> ParserToken -> String) -> Int -> Result String (List ParserToken, (token -> func)) -> Result String (List ParserToken, func)
parserExtract extract errFunc stepNum state =
    Result.andThen
    ( \(s, f) -> case s of
        (t::others) -> extract t
            |> Result.andThen (\tok -> Result.Ok (others, f tok))
            |> Result.mapError (\_ -> errFunc stepNum t)
        [] -> Result.Err "Temporary stack is missing tokens to process the rule"
    )
    state
)""" << endl;
            for (auto [line_no, rule]: raw_rules) {
                if (line_no == 0) continue; // Ignore the rule we generated (it's an identity mapping)
                stream << "parserProcessRule" << line_no << ": ParserState -> Result String ParserState" << endl;
                stream << "parserProcessRule" << line_no << " state = case parserExtractLastN " << rule.second.size() << " (state,[]) of" << endl;
                stream << "    Result.Err _ -> Result.Err \"Parser is missing tokens to process the rule\"" << endl; 
                stream << "    Result.Ok (newState, stack) ->" << endl;
                stream << "        Result.Ok (stack, parserRule" << line_no << ")" << endl;
                for (int i = 0 ; i < rule.second.size(); i++)
                    stream << "        |> parserExtract parserExpect" << rule.second[i]->token_name() << " parserError" << line_no << " " << (i + 1) << endl;;
                stream << "        |> Result.andThen ( \\(_, output) -> case output of" << endl;
                stream << "            Result.Ok o -> Result.Ok {newState | stack = (Parser" << rule.first->token_name() << " o)::newState.stack}" << endl;
                stream << "            Result.Err e -> Result.Err e" << endl;
                stream << "        )" << endl << endl;
            }
            stream << "parserProcessToken: ParserToken -> ParserState -> Result String ParserState" << endl;
            stream << "parserProcessToken token state = case state.states of" << endl;
            stream << "    [] -> Result.Err \"Parser has completed\"" << endl;

            // process the transition table
            queue<transition_state_t> to_process;
            unordered_map<unordered_set<const node_t*>, int, node_t::set_hash> seen;
            auto initial_state = transition_state_t::expand(head, head.find(main_token)->second.first);
            to_process.push(initial_state);
            seen[initial_state.nodes] = 0;
            int state_no = 0;
            while (!to_process.empty()) {
                transition_state_t state = to_process.front(); to_process.pop();
                state.print_case_start(stream, state_no++);
                set<pair<int,const token_t*>> transition_tokens;
                for (auto node : state.nodes) if (node->token != NULL) {
                    // Use the index to keep the ordering deterministic
                    int index = find(token_order.cbegin(), token_order.cend(), node->token)-token_order.cend();
                    transition_tokens.insert({index, node->token});
                }
                for (auto [_, t] : transition_tokens) {
                    auto next_state = state.step(head, t);
                    int next_state_num;
                    if (seen.find(next_state.nodes) != seen.end()) next_state_num = seen[next_state.nodes];
                    else {
                        next_state_num = seen.size();
                        seen[next_state.nodes] = next_state_num;
                        to_process.push(next_state);
                    }
                    stream << "        Parser" << t->token_name() << " _ -> Result.Ok {state | states = (" << next_state_num << "::state.states), stack=(token::state.stack) }" << endl;
                }
                state.print_case_close(stream);
            }
            stream << "    (_::_) -> Result.Err \"Unknown state reached\"" << endl;
            stream << endl;
        }
    };

    vector<const token_t*> token_order;
    token_parser_t token_parser;
    grammar_parser_t grammar_parser;
    unordered_map<string,token_t*> all_tokens;

    parser_t(const vector<vector<string>>& tokenified_lines, const vector<string>& grammar_names, const vector<string>& char_names)
        : token_order(grammar_names.size() + char_names.size(), NULL), token_parser(), grammar_parser(), all_tokens()
    {
        vector<const token_t*>::iterator it = token_order.begin();
        for (const string& c: grammar_names) {
            token_t* id = new grammar_token_t(c);
            all_tokens[c] = id; *it = id; ++it;
        }
        for (int i = 0; i < char_names.size(); i++) {
            char_token_t* id = new char_token_t(i+1, char_names[i]);
            all_tokens[char_names[i]] = id; *it = id; ++it;
            token_parser.add_token(id, char_names[i]);
        }
        
        int rule_no = 0;
        const token_t* rule_output;
        vector<const token_t*> inputs;
        for (int line_no = 0; line_no < tokenified_lines.size(); line_no++ ) {
            const vector<string>& line = tokenified_lines[line_no];
            auto begin = line.cbegin(), end = line.cend();
            if (line.size() > 1 && line[1] == "->") {
                // process previously extracted rule
                if (rule_no != 0) {
                    grammar_parser.add_rules(rule_no, rule_output, inputs);
                    inputs.clear();
                }
                rule_no = line_no + 1;
                const auto return_token = all_tokens.find(line[0]);
                if (return_token == all_tokens.end()) throw "Unknown Rule name in line: " + to_string(line_no + 1);
                rule_output = return_token->second;
                ++begin; ++begin;
            }
            while (begin != end) {
                const auto token = all_tokens.find(*begin);
                if (token == all_tokens.end()) throw "Unknown token '" + *begin +"' in line: " + to_string(line_no + 1);
                inputs.push_back(token->second);
                ++begin;
            }
        }
        if (rule_no != 0) grammar_parser.add_rules(rule_no, rule_output, inputs);
    }
    parser_t(parser_t& o) =delete;
    parser_t(const parser_t& s) =delete;
    parser_t(parser_t&& o):token_order(std::move(o.token_order)),token_parser(std::move(o.token_parser)),all_tokens(std::move(o.all_tokens)) {}
    
    // Transition table for Elm to execute
    void print(ostream& stream) const {
        stream << "-- Tokens" << endl;
        // Define the type
        stream << "type ParserToken =" << endl;
        stream << "    ParserTokenEnd" << endl;
        for (const token_t* t : token_order) stream << "    | Parser" << t->token_name() << " " << t->token_type() << endl;
        stream << endl;
        // Add types' name
        stream << "parserTokenName: ParserToken -> String" << endl;
        stream << "parserTokenName token = case token of" << endl;
        stream << "    ParserTokenEnd -> \"$\"" << endl;
        for (const token_t* t : token_order) stream << "    Parser" << t->token_name() << " _ -> \"" << t->token_raw() << "\"" << endl;
        stream << endl;
        // Add extraction of expected token
        for (const token_t* t : token_order) {
            stream << "parserExpect" << t->token_name() << ": ParserToken -> Result () " << t->token_type() << endl;
            stream << "parserExpect" << t->token_name() << " token = case token of" << endl;
            stream << "    Parser" << t->token_name() << " e -> Result.Ok e" << endl;
            stream << "    _ -> Result.Err ()";
            stream << endl << endl;
        }
        token_parser.print(stream);
        grammar_parser.print(stream, token_order);
    }
};

/* Extracting values from file */
vector<string> read_lines(string filename) {
    ifstream file(filename);
    vector<string> result;
    string line;
    while (getline(file, line)) result.push_back(line);
    return result;
}

parser_t extract_from_lines(const vector<string>& lines) {
    // First pass is to collect all 'tokens'
    // Only pick up the left-tokens for the grammar_map and the 'regex' tokens for the char_set
    vector<vector<string>> tokenified_lines;
    vector<string> grammar_names;
    vector<string> char_names;
    for (const string& line : lines) {
        vector<string> list;
        string::const_iterator begin = line.cbegin(), end = line.cend();
        string prev_token = "";
        while (begin != end) {
            // Find the next non-space
            while (begin != end && *begin == ' ') ++begin;
            if (begin == end) break;
            // Find the next space
            string::const_iterator start = begin;
            if (*begin == '"') {
                ++begin;
                while (begin != end && *begin != '"') {
                    if (*begin == '\\') if (++begin == end) throw "Line stopped in the middle of a string token: " + line;
                    ++begin;
                }
                if (begin == end) throw "Line stopped in the middle of a string token: " + line;
                if (++begin != end && *begin != ' ') throw "Space required after a string token: " + line;
            } else {
                while (begin != end && *begin != ' ') {
                    if (*begin == '\\') if (++begin == end) throw "Line ends with the escape character: " + line;
                    ++begin;
                }
            }
            // Add the token
            string token(start,begin);
            list.push_back(token);
            if (*start == '"') {
                if (find(char_names.cbegin(), char_names.cend(), token) == char_names.cend()) char_names.push_back(token);
            } else if (token == "->") {
                if (prev_token == "") throw "Missing Rule name on the left of -> in: " + line;
                if (prev_token == "->") throw "Double -> found consecutively in the same line: " + line;
                if (prev_token.front() == '"') throw "StringToken cannot be the name of a rule in line: "+line;
                if (find(grammar_names.begin(),grammar_names.end(), prev_token) == grammar_names.end()) grammar_names.push_back(prev_token);
            }
            prev_token = token;
        }
        tokenified_lines.push_back(list);
    }
    return parser_t(tokenified_lines, grammar_names, char_names);
}

void print_lines(ostream& stream, const vector<string>& lines) {
    stream << "{-" << endl;
    for (const string& l : lines) {
        stream << l << endl;
    }
    stream << "-}" << endl << endl;
}

/* main */
int main(int argc, char **argv) {
    try {
        if (argc == 1) throw "Filename must be specified as an argument";
        if (argc > 2) throw "Multiple arugments passed. This only accepts exactly one input: the filename of the grammar";
        vector<string> lines = read_lines(argv[1]);
        parser_t parser = extract_from_lines(lines);
        print_lines(cout, lines);
        parser.print(cout);
    } catch (const string& s) {
        cerr << s << endl;
        return 1;
    } catch (char const * c) {
        cerr << c << endl;
        return 1;
    }
    return 0;
}