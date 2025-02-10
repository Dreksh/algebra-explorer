#include <algorithm>
#include <iostream>
#include <fstream>
#include <queue>
#include <set> // tokens require sorting for making the output deterministic
#include <string>
#include <sstream>
#include <unordered_map>
#include <unordered_set> // For unordered_set
#include <utility> // For pair
#include <vector>

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
        static string escape_quotes(string raw) {
            // from https://stackoverflow.com/a/24315631
            for (size_t start_pos = 0; (start_pos = raw.find("\"", start_pos)) != string::npos ; ) {
                raw.replace(start_pos, 1, "\\\"");
                start_pos += 2; // Handles case where 'to' is a substring of 'from'
            }
            return raw; 
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
                    size_t result;
                    for (auto ptr : s) result ^= hasher((void*)ptr);
                    return result;
                }
            };
            node_t *next_node;  // from specifying specific characters
            vector<pair<char,char>> next; // character set for going to next
            bool allow_skip;    // from specifying *, this will include next_node->next_state(..) in the calculations
            bool allow_others; // from specifying \o
            bool allow_repeat; // from specifying + or *
            const char_token_t* end;
            node_t(): next_node(NULL), next(), allow_skip(), allow_others(false), end(NULL) {}
            node_t(node_t& t) =delete;
            node_t(const node_t& t) =delete;
            ~node_t() { if (next_node != NULL) delete next_node; }
            unordered_set<const node_t*> next_state(pair<char,char> token) const {
                bool found = false;
                if (token == pair<char,char>('\0', '\0')) {
                    found = allow_others;
                } else {
                    for (auto [low, high]: next) {
                        if (low >= token.first && low <= token.second) {
                            if (high > token.second) throw "token is out of range";
                            found = true;
                            break;
                        } else if (high >= token.first && high <= token.second) {
                            throw "token is out of range";
                        }
                    }
                }
                unordered_set<const node_t*> s;
                if (found) s.insert(next_node);
                if (allow_repeat) s.insert(this);
                if (allow_skip && next_node != NULL) s.merge(next_node->next_state(token));
                return s;
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
            node_t* prev_node = new node_t, *current_node = prev_node;
            this->roots.insert(prev_node);
            while (begin != end) {
                switch (*begin) {
                    case '[': { // start multirange
                        char prev('\0');
                        ++begin;
                        while (begin != end && *begin != ']') {
                            switch (*begin) {
                            case '\\': // start escape
                                if (prev != '\0') { current_node->next.push_back({prev, prev}); all_tokens.insert({prev,prev}); }
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
                                    case '\\': // start escape
                                        ++begin;
                                        if (begin == end) throw "Unexpected end of the token definition in: " + input;
                                        if (*begin == 'o') {
                                            current_node->next.push_back({prev,prev}); all_tokens.insert({prev,prev});
                                            current_node->next.push_back({'-','-'}); all_tokens.insert({'-','-'});
                                            current_node->allow_others = true;
                                        } else {
                                            current_node->next.push_back({prev,*begin}); all_tokens.insert({prev,*begin});
                                        }
                                        prev = '\0';
                                        ++begin;
                                        break;
                                    case ']': // end of group
                                        current_node->next.push_back({prev,prev}); all_tokens.insert({prev,prev});
                                        current_node->next.push_back({'-','-'}); all_tokens.insert({'-','-'});
                                        break;
                                    default: { // any other char
                                        current_node->next.push_back({prev,prev}); all_tokens.insert({prev, prev});
                                        prev = '\0';
                                        ++begin;
                                    }
                                }
                                break;
                            default: // anything else
                                if (prev != '\0') { current_node->next.push_back({prev,prev}); all_tokens.insert({prev,prev}); }
                                prev = *begin;
                                ++begin;
                            }
                        }
                        if (begin == end) throw "Unexpected end of token definition in: " + input;
                        prev_node = current_node; current_node = prev_node->next_node = new node_t;
                        ++begin;
                        }
                        break;
                    case '\\': // start escape
                        ++begin;
                        if (begin == end) throw "Unexpected end of token definition in: " + input;
                        if (*begin == 'o') current_node->allow_others = true;
                        else { current_node->next.push_back({*begin,*begin}); all_tokens.insert({*begin,*begin}); }
                        prev_node = current_node; current_node = prev_node->next_node = new node_t;
                        ++begin;
                        break;
                    case '+': // repeat itself as 
                        if (prev_node == current_node) throw "Unable to use '+' without any prefix in: " + input;
                        prev_node->allow_repeat = true;
                        ++begin;
                        break;
                    case '*': // repeat itself as 
                        if (prev_node == current_node) throw "Unable to use '*' without any prefix in: " + input;
                        prev_node->allow_skip = true;
                        ++begin;
                        break;
                    case '?': // repeat itself as 
                        if (prev_node == current_node) throw "Unable to use '?' without any prefix in: " + input;
                        prev_node->allow_repeat = true;
                        prev_node->allow_skip = true;
                        ++begin;
                        break;
                    case '"': // terminal
                        if (++begin != end) throw "Expected end after '\"' in: " + input;
                        break;
                    default:
                        current_node->next.push_back({*begin,*begin}); all_tokens.insert({*begin,*begin});
                        prev_node = current_node; current_node = prev_node->next_node = new node_t;
                        ++begin;
                }
            }
            current_node->end = id;
        }
        // Printing related
        vector<pair<char,char>> unique_char_ranges() const {
            if (all_tokens.empty()) return vector<pair<char,char>>();
            vector<pair<char,char>> confirmed;
            char prev_low;
            set<char> upper_bounds;
            // This algorithm expects the iteration of all_tokens to be ordered
            for (auto [low, high] : all_tokens) {
                // no lingering range from before
                if (upper_bounds.empty()) { prev_low = low; upper_bounds.insert(high); }
                // lingering range from before
                else {
                    auto begin = upper_bounds.begin(), end = upper_bounds.end();
                    // Process all ranges that can be cleared
                    while (begin != end && low > *begin) { confirmed.push_back({prev_low, *begin}); prev_low = *begin+1; ++begin; }
                    // no more ranges to clear
                    if (begin == end) { prev_low = low; upper_bounds.insert(high); }
                    // insert regions into upper_bounds
                    else {
                        upper_bounds.erase(upper_bounds.begin(), begin);
                        upper_bounds.insert(high);
                        // stop before the lower-bound, since that's the start of the next token
                        if (prev_low != low) upper_bounds.insert(low-1);
                    }
                }
            }
            for (char next: upper_bounds) { confirmed.push_back({prev_low, next}); prev_low = next+1; }
            return confirmed;
        }
        static void print_char_set(ostream& stream, pair<char,char> tok) {
            stream << '"';
            for (char i = tok.first; i <= tok.second; i++) { if (i == '"') stream << '\\'; stream << i; }
            stream << '"';
        }
        static void print_all_char_set(ostream& stream, const vector<pair<char,char>>& all_chars) {
            stream << '"';
            for (auto tok: all_chars) for (char i = tok.first; i <= tok.second; i++) { if (i == '"') stream << '\\'; stream << i; }
            stream << '"';
        }
        void print(ostream& stream) const {
            vector<pair<char,char>> chars = unique_char_ranges();
            stream << R"""(
            -- TokenState
            type alias ParserState Token =
                {   seen: String
                ,   state: Int
                }
            )
            
            parserProcessChar: ParserStateToken -> String -> (ParserStateToken, Maybe ParserToken)
            parserProcessChar state char = case state.state of
            )""" << endl;

            stringstream end_stream;
            int state(0);
            unordered_map<unordered_set<const node_t*>, int, node_t::set_hash> seen;
            queue<unordered_set<const node_t*>> to_process;
            seen[roots] = 0;
            to_process.push(roots);
            while (!to_process.empty()) {
                unordered_set<const node_t*> current = to_process.front();
                to_process.pop();
                stream << "    " << state << " -> ";
                // Add transitions
                for (pair<char,char> char_set : chars) {
                    unordered_set<const node_t*> next;
                    for (const node_t* node : current) next.merge(node->next_state(char_set));
                    if (next.empty()) continue;
                    stream << "if String.contains char ";
                    token_parser_t::print_char_set(stream, char_set);
                    stream << endl;
                    int next_num;
                    unordered_map<unordered_set<const node_t*>, int, node_t::set_hash>::iterator loc = seen.find(next);
                    if (loc != seen.end()) next_num = loc->second;
                    else {
                        next_num = seen.size();
                        seen[next] = next_num;
                        to_process.push(next);
                    }
                    stream << "        then ({state | seen = state.seen ++ char, state = " << next_num << "}, Nothing)" << endl;
                    stream << "        else ";
                }
                // Check for others
                unordered_set<const node_t*> next;
                for (const node_t* node : current) if (node->allow_others) next.merge(node->next_state({'\0','\0'}));
                if (!next.empty()) {
                    int next_num;
                    unordered_map<unordered_set<const node_t*>, int, node_t::set_hash>::iterator loc = seen.find(next);
                    if (loc != seen.end()) next_num = loc->second;
                    else {
                        next_num = seen.size();
                        seen[next] = next_num;
                        to_process.push(next);
                    }
                    stream << "if String.contains char "; token_parser_t::print_all_char_set(stream, chars);
                    stream << " |> not" << endl;
                    stream << "        then ({state | seen = state.seen ++ char, state = " << next_num << "}, Nothing)" << endl;
                    stream << "        else ";
                }
                // Check it uniquely identifies a token
                unordered_set<const char_token_t*> end_tokens;
                for (const node_t* node : current) if (node->end != NULL) end_tokens.insert(node->end);
                if (end_tokens.size() > 1) {
                    string list;
                    for (const char_token_t* node : end_tokens) list += " " + node->token_raw();
                    throw "token can match multiple tokens in:" + list;
                }
                // Add termination case
                stream << "let (newState, _) = parserProcessChar {seen = \"\", state = 0} char in (newState, ";
                unordered_set<const char_token_t*>::const_iterator ptr = end_tokens.cbegin();
                if (ptr != end_tokens.cend()) {
                    end_stream << "    " << state << " -> Just (Parser" << (*ptr)->token_name() << " state.seen)" << endl;
                    stream << "Just (Parser" << (*ptr)->token_name() << " state.seen))" << endl;
                } else stream << "Nothing)" << endl;
            }
            stream << R"""(
                _ -> ({seen = \"\", state = 0}, Nothing);
            
            parserProcessEnd: ParserStateToken -> Maybe ParserToken
            parserProcessEnd state = case state.state of
            )""" << endl;
            stream << end_stream.str();
            stream << "    _ -> Nothing" << endl << endl;
        }
    };
    struct grammar_parser_t {
        struct node_t {
            struct set_hash {
                size_t operator() (const unordered_set<const node_t*>& s) const {
                    hash<void*> hasher;
                    size_t result;
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
                else stream << "        other -> parserProcessRule" << end_no << " state | states=states} |> Result.andThen (\\s -> parserProcessToken other s)" << endl;
            }
            transition_state_t step(const unordered_map<const token_t*,pair<unordered_set<const node_t*>,bool>>& dict, const token_t* t) {
                unordered_set<const node_t*> next;
                for (auto n : nodes) if (n->token == t) next.insert(n->next);
                return transition_state_t::expand(dict, next);
            }
            static transition_state_t expand(const unordered_map<const token_t*,pair<unordered_set<const node_t*>,bool>>& dict, unordered_set<const node_t*> set) {
                queue<const node_t*> to_visit;
                for (auto ptr : set) to_visit.push(ptr);
                int end_no(-1), rule_no, input_no;
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
                    } else {
                        // Find new rules with epsilon in the next token
                        auto rule = dict.find(next->token);
                        if (rule != dict.end() && rule->second.second && set.find(next->next) == set.end()) {
                            to_visit.push(next->next);
                            set.insert(next->next);
                        }
                    }
                }
                return transition_state_t(std::move(set), end_no, rule_no, input_no);
            }
        };

        const token_t* main_token;
        unordered_map<const token_t*,pair<unordered_set<const node_t*>,bool>> head;
        vector<pair<int,pair<const token_t*,vector<const token_t*>>>> rules;

        grammar_parser_t(): main_token(NULL), head(), rules() {}
        grammar_parser_t(grammar_parser_t& o) =delete;
        grammar_parser_t(const grammar_parser_t& o) =delete;
        grammar_parser_t(grammar_parser_t&& o): main_token(o.main_token), head(std::move(o.head)), rules(std::move(o.rules)) {}
        ~grammar_parser_t() { for (auto [_, all_start]: head) for (auto ptr: all_start.first) delete ptr; }

        void add_rules(int line_no, const token_t* return_token, const vector<const token_t*>& tokens) {
            if (main_token == NULL) {
                // Add the default rule
                main_token = return_token;
                rules.push_back({0,{return_token,{return_token}}});
            }
            if (tokens.empty()) { head[return_token].second = true; return; }
            rules.push_back({line_no, {return_token, tokens}});
            node_t *ptr, **ptr_ptr = &ptr;
            int input_no = 1;
            for (const token_t* t: tokens) { *ptr_ptr = new node_t(t, line_no, input_no++); ptr_ptr = &((*ptr_ptr)->next); }
            // Add a final node for specifying the end of the rule
            *ptr_ptr = new node_t(NULL, line_no, 0);
            head[return_token].first.insert(ptr);
        }
        void print(ostream& stream, const vector<const token_t*>& token_order) const {
            if (main_token == NULL) throw "No rules are registered";
            stream << R"""(
                -- Transitions
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
                    r -> case parserExtractLastN (r-1) (state,stack) of
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
            for (auto [line_no, rule]: rules) {
                stream << "parserProcessRule" << line_no << ": ParserState -> Result String ParserState" << endl;
                stream << "parserProcessRule" << line_no << " state = case parserExtractLastN " << rule.second.size() << " (state,[]) of" << endl;
                stream << "    Result.Err _ -> Result.Err \"Parser is missing tokens to process the rule\"" << endl; 
                stream << "    Result.Ok (newState, stack) ->" << endl;
                stream << "        Result.Ok (stack, parserRule" << line_no << ")" << endl;
                for (int i = 0 ; i < rule.second.size(); i++) stream << "        |> parserExtract parserExpectTokenExpression parserError" << line_no << " " << (i + 1) << endl;;
                stream << "        |> Result.andThen ( \(_, output) -> case output of" << endl;
                stream << "            Result.Ok o -> Result.Ok {newState | stack = (Parser" << rule.first->token_name() << " o)::newState.stack}" << endl;
                stream << "            Result.Err e -> Result.Err e" << endl;
                stream << "        )" << endl << endl;
            }
            stream << "parserProcessToken: ParserToken -> ParserState -> Result String ParserState" << endl;
            stream << "parserProcessToken token state = case state.state of" << endl;
            stream << "    [] -> Result.Err \"Parser has completed\"" << endl;

            // process the transition table
            queue<transition_state_t> to_process;
            unordered_map<unordered_set<const node_t*>, int, node_t::set_hash> seen;
            auto initial_state = transition_state_t::expand(head, head.find(main_token)->second.first);
            to_process.push(initial_state);
            seen[initial_state.nodes] = 0;
            int state_no = 0;
            while (!to_process.empty()) {
                auto state = to_process.front();
                state.print_case_start(stream, state_no);
                set<pair<int,const token_t*>> tokens;
                for (auto node : state.nodes) if (node->token != NULL) {
                    // Use the index to keep the ordering deterministic
                    int index = find(token_order.cbegin(), token_order.cend(), node->token)-token_order.cend();
                    tokens.insert({index, node->token});
                }
                for (auto [_, t] : tokens) {
                    auto next_state = state.step(head, t);
                    int next_state_num;
                    if (seen.find(next_state.nodes) != seen.end()) next_state_num = seen[next_state.nodes];
                    else {
                        next_state_num = seen.size();
                        seen[next_state.nodes] = next_state_num;
                        to_process.push(next_state);
                    }
                    stream << "        Parser" << t->token_name() << " -> Result.Ok {state | states = (" << next_state_num << "::state.states) }" << endl;
                }
                for (const token_t* token : token_order) {

                }
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
                    rule_no = line_no + 1;
                    inputs.clear();
                }
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
        for (const token_t* t : token_order) stream << "    Parser" << t->token_name() << " -> \"" << t->token_type() << "\"" << endl;
        stream << endl;
        // Add extraction of expected token
        for (const token_t* t : token_order) {
            stream << "parserExpect" << t->token_name() << ": ParserToken -> Result () " << t->token_type() << endl;
            stream << "parserExpect" << t->token_name() << " token = case token of" << endl;
            stream << "    Parser" << t->token_name() << " e -> Result.Ok e" << endl;
            stream << "    _ -> Result.Err ()";
            stream << endl;
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
    vector<vector<string>> tokenified_lines(lines.size());
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
            while (begin != end && *begin != ' ') {
                if (*begin == '\'') {
                    ++begin;
                    if (begin == end) throw "Line ends with the escape character: " + line;
                }
                ++begin;
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
                grammar_names.push_back(prev_token);
            }
            prev_token = token;
        }
    }
    return parser_t(tokenified_lines, grammar_names, char_names);
}

void print_lines(ostream& stream, const vector<string>& lines) {
    stream << "-- Grammar input" << endl;
    stream << "parserGrammer: String" << endl;
    stream << "parserGrammer =" << endl;
    stream << "\"\"\"" << endl;
    for (const string& l : lines) {
        stream << l << endl;
    }
    stream << "\"\"\"" << endl << endl;
}

/* main */
int main(int argc, char **argv) {
    if (argc != 2) {
        cerr << "Filename must be specified" << endl;
        return 1;
    }
    try {
        vector<string> lines = read_lines(string(argv[1]));
        parser_t parser = extract_from_lines(lines);
        print_lines(cout, lines);
        parser.print(cout);
    } catch (const string& s) {
        cerr << s << endl;
        return 1;
    }
    return 0;
}
