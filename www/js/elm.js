(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.ap.T === region.aB.T)
	{
		return 'on line ' + region.ap.T;
	}
	return 'on lines ' + region.ap.T + ' through ' + region.aB.T;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bp,
		impl.bF,
		impl.bC,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		y: func(record.y),
		aq: record.aq,
		ak: record.ak
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.y;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aq;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.ak) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bp,
		impl.bF,
		impl.bC,
		function(sendToApp, initialModel) {
			var view = impl.bH;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bp,
		impl.bF,
		impl.bC,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.ao && impl.ao(sendToApp)
			var view = impl.bH;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.bf);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bE) && (_VirtualDom_doc.title = title = doc.bE);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bs;
	var onUrlRequest = impl.bt;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		ao: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.aU === next.aU
							&& curr.aJ === next.aJ
							&& curr.aR.a === next.aR.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		bp: function(flags)
		{
			return A3(impl.bp, flags, _Browser_getUrl(), key);
		},
		bH: impl.bH,
		bF: impl.bF,
		bC: impl.bC
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { bn: 'hidden', bg: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { bn: 'mozHidden', bg: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { bn: 'msHidden', bg: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { bn: 'webkitHidden', bg: 'webkitvisibilitychange' }
		: { bn: 'hidden', bg: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		a$: _Browser_getScene(),
		a8: {
			ba: _Browser_window.pageXOffset,
			bb: _Browser_window.pageYOffset,
			a9: _Browser_doc.documentElement.clientWidth,
			aH: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		a9: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		aH: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			a$: {
				a9: node.scrollWidth,
				aH: node.scrollHeight
			},
			a8: {
				ba: node.scrollLeft,
				bb: node.scrollTop,
				a9: node.clientWidth,
				aH: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			a$: _Browser_getScene(),
			a8: {
				ba: x,
				bb: y,
				a9: _Browser_doc.documentElement.clientWidth,
				aH: _Browser_doc.documentElement.clientHeight
			},
			bk: {
				ba: x + rect.left,
				bb: y + rect.top,
				a9: rect.width,
				aH: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.bl.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.bl.b, xhr)); });
		$elm$core$Maybe$isJust(request.a6) && _Http_track(router, xhr, request.a6.a);

		try {
			xhr.open(request.br, request.bG, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.bG));
		}

		_Http_configureRequest(xhr, request);

		request.bf.a && xhr.setRequestHeader('Content-Type', request.bf.a);
		xhr.send(request.bf.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.aG; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.bD.a || 0;
	xhr.responseType = request.bl.d;
	xhr.withCredentials = request.bd;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		bG: xhr.responseURL,
		bA: xhr.status,
		bB: xhr.statusText,
		aG: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			bz: event.loaded,
			a2: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			bx: event.loaded,
			a2: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}


// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}

var $author$project$Main$EventUrlChange = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$EventUrlRequest = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.e) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.f),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.f);
		} else {
			var treeLen = builder.e * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.g) : builder.g;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.e);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.f) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.f);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{g: nodeList, e: (len / $elm$core$Array$branchFactor) | 0, f: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {aF: fragment, aJ: host, aP: path, aR: port_, aU: protocol, ad: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Main$NotificationEvent = function (a) {
	return {$: 5, a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$Notification$ClearEvent = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Notification$delayedClear_ = function (id) {
	return A2(
		$elm$core$Task$perform,
		function (_v0) {
			return $author$project$Notification$ClearEvent(id);
		},
		$elm$core$Process$sleep(15000));
};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$Notification$displayError = F2(
	function (str, _v0) {
		var model = _v0.a;
		var cmd = _v0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					M: model.M + 1,
					w: A3(
						$elm$core$Dict$insert,
						model.M,
						_Utils_Tuple2(false, str),
						model.w)
				}),
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						cmd,
						$author$project$Notification$delayedClear_(model.M)
					])));
	});
var $author$project$Main$NoOp = {$: 7};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			A2(
				$elm$core$Task$onError,
				A2(
					$elm$core$Basics$composeL,
					A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
					$elm$core$Result$Err),
				A2(
					$elm$core$Task$andThen,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Ok),
					task)));
	});
var $elm$browser$Browser$Dom$focus = _Browser_call('focus');
var $author$project$Main$focusTextBar_ = function (id) {
	return A2(
		$elm$core$Task$attempt,
		function (_v0) {
			return $author$project$Main$NoOp;
		},
		$elm$browser$Browser$Dom$focus(id));
};
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Math$BinaryNode = function (a) {
	return {$: 3, a: a};
};
var $author$project$Math$GenericNode = function (a) {
	return {$: 4, a: a};
};
var $author$project$Math$RealNode = function (a) {
	return {$: 0, a: a};
};
var $author$project$Math$UnaryNode = function (a) {
	return {$: 2, a: a};
};
var $author$project$Math$VariableNode = function (a) {
	return {$: 1, a: a};
};
var $author$project$Display$processIDChild_ = F3(
	function (parent, oldRoot, _v8) {
		var parentMap = _v8.a;
		var result = _v8.b;
		var _v9 = A3($author$project$Display$processID_, parent, parentMap, oldRoot);
		var newMap = _v9.a;
		var newChild = _v9.b;
		return _Utils_Tuple2(
			newMap,
			_Utils_ap(
				result,
				_List_fromArray(
					[newChild])));
	});
var $author$project$Display$processID_ = F3(
	function (parent, _v0, oldRoot) {
		var next = _v0.a;
		var map = _v0.b;
		switch (oldRoot.$) {
			case 0:
				var s = oldRoot.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						next + 1,
						A3($elm$core$Dict$insert, next, parent, map)),
					$author$project$Math$RealNode(
						{
							J: {
								o: next,
								N: _Utils_Tuple2(0, 0)
							},
							ar: s.ar
						}));
			case 1:
				var s = oldRoot.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						next + 1,
						A3($elm$core$Dict$insert, next, parent, map)),
					$author$project$Math$VariableNode(
						{
							m: s.m,
							J: {
								o: next,
								N: _Utils_Tuple2(0, 0)
							}
						}));
			case 2:
				var s = oldRoot.a;
				var _v2 = A3(
					$author$project$Display$processID_,
					next,
					_Utils_Tuple2(next + 1, map),
					s.aw);
				var _v3 = _v2.a;
				var newNext = _v3.a;
				var newDict = _v3.b;
				var newChild = _v2.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						newNext,
						A3($elm$core$Dict$insert, next, parent, newDict)),
					$author$project$Math$UnaryNode(
						{
							aw: newChild,
							m: s.m,
							J: {
								o: next,
								N: _Utils_Tuple2(0, 0)
							}
						}));
			case 3:
				var s = oldRoot.a;
				var _v4 = A3(
					$elm$core$List$foldl,
					$author$project$Display$processIDChild_(next),
					_Utils_Tuple2(
						_Utils_Tuple2(next + 1, map),
						_List_Nil),
					s.S);
				var _v5 = _v4.a;
				var finalNext = _v5.a;
				var finalMap = _v5.b;
				var newChildren = _v4.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						finalNext,
						A3($elm$core$Dict$insert, next, parent, finalMap)),
					$author$project$Math$BinaryNode(
						{
							at: s.at,
							S: newChildren,
							ax: s.ax,
							m: s.m,
							J: {
								o: next,
								N: _Utils_Tuple2(0, 0)
							}
						}));
			default:
				var s = oldRoot.a;
				var _v6 = A3(
					$elm$core$List$foldl,
					$author$project$Display$processIDChild_(next),
					_Utils_Tuple2(
						_Utils_Tuple2(next + 1, map),
						_List_Nil),
					s.S);
				var _v7 = _v6.a;
				var finalNext = _v7.a;
				var finalMap = _v7.b;
				var newChildren = _v6.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						finalNext,
						A3($elm$core$Dict$insert, next, parent, finalMap)),
					$author$project$Math$GenericNode(
						{
							S: newChildren,
							m: s.m,
							J: {
								o: next,
								N: _Utils_Tuple2(0, 0)
							}
						}));
		}
	});
var $author$project$Display$addEquation = F2(
	function (tree, model) {
		var eq = A3(
			$author$project$Display$processID_,
			-1,
			_Utils_Tuple2(0, $elm$core$Dict$empty),
			tree);
		return _Utils_update(
			model,
			{
				F: A3($elm$core$Dict$insert, model.V, eq, model.F),
				V: model.V + 1
			});
	});
var $author$project$Display$init = A2(
	$elm$core$List$foldl,
	$author$project$Display$addEquation,
	{ay: $elm$core$Maybe$Nothing, F: $elm$core$Dict$empty, V: 0, x: $elm$core$Maybe$Nothing});
var $author$project$Menu$init = function (shown) {
	return {B: shown};
};
var $author$project$Notification$init = {M: 0, w: $elm$core$Dict$empty};
var $author$project$Rules$init = {p: $elm$core$Dict$empty, q: false, r: $elm$core$Dict$empty, C: $elm$core$Dict$empty};
var $author$project$Tutorial$init = {aN: 0, a0: 0};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Math$deadEndToString_ = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$map(
		function (deadEnd) {
			return function (str) {
				return str + (', at position: ' + $elm$core$String$fromInt(deadEnd.bh));
			}(
				function () {
					var _v0 = deadEnd.bw;
					switch (_v0.$) {
						case 0:
							var str = _v0.a;
							return 'Expecting \'' + (str + '\'');
						case 1:
							return 'Expecting a whole number';
						case 2:
							return 'Expecting a hex number';
						case 3:
							return 'Expecting an octal number';
						case 4:
							return 'Expecting a binary number';
						case 5:
							return 'Expecting a decimal number';
						case 6:
							return 'Expecting a number';
						case 7:
							return 'Expecting a variable';
						case 8:
							var str = _v0.a;
							return 'Expecting \'' + (str + '\'');
						case 9:
							var str = _v0.a;
							return 'Expecting \'' + (str + '\'');
						case 10:
							return 'Expecting no more characters';
						case 11:
							return 'Unknown symbol';
						case 12:
							var str = _v0.a;
							return str;
						default:
							return 'Bad repeat';
					}
				}());
		}),
	$elm$core$String$join('\n'));
var $author$project$Math$createErrorMessage_ = F2(
	function (str, err) {
		return 'Error parsing \"' + (str + ('\":\n' + $author$project$Math$deadEndToString_(err)));
	});
var $elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
			var _v2 = parseA(s0);
			if (_v2.$ === 1) {
				var p = _v2.a;
				var x = _v2.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v2.a;
				var a = _v2.b;
				var s1 = _v2.c;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$parser$Parser$Advanced$lazy = function (thunk) {
	return function (s) {
		var _v0 = thunk(0);
		var parse = _v0;
		return parse(s);
	};
};
var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0;
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (!step.$) {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (!step.$) {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (!_v1.$) {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.a);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.b, offset) < 0,
					0,
					{bh: col, c: s0.c, d: s0.d, b: offset, a_: row, a: s0.a});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.b, s.a_, s.bh, s);
	};
};
var $elm$parser$Parser$Advanced$spaces = $elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return (c === ' ') || ((c === '\n') || (c === '\r'));
	});
var $elm$parser$Parser$spaces = $elm$parser$Parser$Advanced$spaces;
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {bh: col, bi: contextStack, bw: problem, a_: row};
	});
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.a_, s.bh, x, s.c));
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.b, s.a_, s.bh, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{bh: newCol, c: s.c, d: s.d, b: newOffset, a_: newRow, a: s.a});
	};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $author$project$Math$validVarStart_ = function (_char) {
	return !A2(
		$elm$core$String$contains,
		$elm$core$String$fromChar(_char),
		' ~+-=*/\\.()[],;%!:;<>0123456789^&|$↔→');
};
var $elm$parser$Parser$ExpectingVariable = {$: 7};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$parser$Parser$Advanced$varHelp = F7(
	function (isGood, offset, row, col, src, indent, context) {
		varHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, src);
			if (_Utils_eq(newOffset, -1)) {
				return {bh: col, c: context, d: indent, b: offset, a_: row, a: src};
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$variable = function (i) {
	return function (s) {
		var firstOffset = A3($elm$parser$Parser$Advanced$isSubChar, i.ap, s.b, s.a);
		if (_Utils_eq(firstOffset, -1)) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.aD));
		} else {
			var s1 = _Utils_eq(firstOffset, -2) ? A7($elm$parser$Parser$Advanced$varHelp, i.af, s.b + 1, s.a_ + 1, 1, s.a, s.d, s.c) : A7($elm$parser$Parser$Advanced$varHelp, i.af, firstOffset, s.a_, s.bh + 1, s.a, s.d, s.c);
			var name = A3($elm$core$String$slice, s.b, s1.b, s.a);
			return A2($elm$core$Set$member, name, i.al) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.aD)) : A3($elm$parser$Parser$Advanced$Good, true, name, s1);
		}
	};
};
var $elm$parser$Parser$variable = function (i) {
	return $elm$parser$Parser$Advanced$variable(
		{aD: $elm$parser$Parser$ExpectingVariable, af: i.af, al: i.al, ap: i.ap});
};
var $author$project$Math$tokenLongName_ = $elm$parser$Parser$variable(
	{
		af: function (c) {
			return $author$project$Math$validVarStart_(c) || $elm$core$Char$isAlphaNum(c);
		},
		al: $elm$core$Set$empty,
		ap: $author$project$Math$validVarStart_
	});
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0;
		return function (s0) {
			var _v1 = parseA(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				var _v2 = callback(a);
				var parseB = _v2;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
				}
			}
		};
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$Problem = function (a) {
	return {$: 12, a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return function (s) {
		return A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Math$tokenDigit_ = $elm$parser$Parser$variable(
	{af: $elm$core$Char$isDigit, al: $elm$core$Set$empty, ap: $elm$core$Char$isDigit});
var $author$project$Math$tokenNumber_ = A2(
	$elm$parser$Parser$andThen,
	function (str) {
		var _v0 = $elm$core$String$toFloat(str);
		if (_v0.$ === 1) {
			return $elm$parser$Parser$problem(str + ' is not a valid number');
		} else {
			var f = _v0.a;
			return $elm$parser$Parser$succeed(
				$author$project$Math$RealNode(
					{J: 0, ar: f}));
		}
	},
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed(
				F2(
					function (a, b) {
						return _Utils_ap(a, b);
					})),
			$author$project$Math$tokenDigit_),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(
							function (b) {
								return '.' + b;
							}),
						$elm$parser$Parser$symbol('.')),
					$author$project$Math$tokenDigit_),
					$elm$parser$Parser$succeed('')
				]))));
var $author$project$Math$tokenShortName_ = $elm$parser$Parser$variable(
	{
		af: function (_v0) {
			return false;
		},
		al: $elm$core$Set$empty,
		ap: $author$project$Math$validVarStart_
	});
function $author$project$Math$cyclic$negatable_() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed(
							function (x) {
								return $author$project$Math$UnaryNode(
									{aw: x, m: '-', J: 0});
							}),
						$elm$parser$Parser$symbol('-')),
					$elm$parser$Parser$spaces),
				$elm$parser$Parser$lazy(
					function (_v7) {
						return $author$project$Math$cyclic$negatable_();
					})),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$author$project$Math$cyclic$term_())
			]));
}
function $author$project$Math$cyclic$term_() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed($elm$core$Basics$identity),
					$elm$parser$Parser$symbol('(')),
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$author$project$Math$cyclic$expression_(),
						$elm$parser$Parser$spaces),
					$elm$parser$Parser$symbol(')'))),
				$author$project$Math$tokenNumber_,
				A2(
				$elm$parser$Parser$map,
				function (_v5) {
					var name = _v5.a;
					var props = _v5.b;
					if (props.$ === 1) {
						return $author$project$Math$VariableNode(
							{m: name, J: 0});
					} else {
						var children = props.a;
						return $author$project$Math$GenericNode(
							{S: children, m: name, J: 0});
					}
				},
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed(
								F2(
									function (a, b) {
										return _Utils_Tuple2(a, b);
									})),
							$elm$parser$Parser$symbol('\\')),
						$author$project$Math$tokenLongName_),
					$author$project$Math$cyclic$varOrFunc_())),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					function (name) {
						return $author$project$Math$VariableNode(
							{m: name, J: 0});
					}),
				$author$project$Math$tokenShortName_)
			]));
}
function $author$project$Math$cyclic$varOrFunc_() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed($elm$core$Maybe$Just),
					$elm$parser$Parser$symbol('(')),
				A2(
					$elm$parser$Parser$ignorer,
					$author$project$Math$cyclic$argsList_(),
					$elm$parser$Parser$symbol(')'))),
				$elm$parser$Parser$succeed($elm$core$Maybe$Nothing)
			]));
}
function $author$project$Math$cyclic$argsList_() {
	return A2(
		$elm$parser$Parser$loop,
		_List_Nil,
		function (list) {
			return $elm$core$List$isEmpty(list) ? A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					A2($elm$core$Basics$composeR, $elm$core$List$singleton, $elm$parser$Parser$Loop)),
				A2(
					$elm$parser$Parser$ignorer,
					$author$project$Math$cyclic$expression_(),
					$elm$parser$Parser$spaces)) : $elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed(
								function (elem) {
									return $elm$parser$Parser$Loop(
										A2($elm$core$List$cons, elem, list));
								}),
							$elm$parser$Parser$symbol(',')),
						A2(
							$elm$parser$Parser$ignorer,
							$author$project$Math$cyclic$expression_(),
							$elm$parser$Parser$spaces)),
						A2(
						$elm$parser$Parser$map,
						function (_v4) {
							return $elm$parser$Parser$Done(
								$elm$core$List$reverse(list));
						},
						$elm$parser$Parser$succeed(0))
					]));
		});
}
function $author$project$Math$cyclic$expression_() {
	return A2(
		$elm$parser$Parser$map,
		function (children) {
			if (children.b && (!children.b.b)) {
				var x = children.a;
				return x;
			} else {
				return $author$project$Math$BinaryNode(
					{at: true, S: children, ax: true, m: '+', J: 0});
			}
		},
		A2(
			$elm$parser$Parser$loop,
			_List_Nil,
			function (list) {
				return $elm$core$List$isEmpty(list) ? A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed(
						A2($elm$core$Basics$composeR, $elm$core$List$singleton, $elm$parser$Parser$Loop)),
					A2(
						$elm$parser$Parser$ignorer,
						$author$project$Math$cyclic$multiple_(),
						$elm$parser$Parser$spaces)) : $elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									function (elem) {
										return $elm$parser$Parser$Loop(
											A2($elm$core$List$cons, elem, list));
									}),
								$elm$parser$Parser$symbol('+')),
							A2(
								$elm$parser$Parser$ignorer,
								$author$project$Math$cyclic$multiple_(),
								$elm$parser$Parser$spaces)),
							A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									function (elem) {
										return $elm$parser$Parser$Loop(
											A2(
												$elm$core$List$cons,
												$author$project$Math$UnaryNode(
													{aw: elem, m: '-', J: 0}),
												list));
									}),
								$elm$parser$Parser$symbol('-')),
							A2(
								$elm$parser$Parser$ignorer,
								$author$project$Math$cyclic$multiple_(),
								$elm$parser$Parser$spaces)),
							A2(
							$elm$parser$Parser$map,
							function (_v2) {
								return $elm$parser$Parser$Done(
									$elm$core$List$reverse(list));
							},
							$elm$parser$Parser$succeed(0))
						]));
			}));
}
function $author$project$Math$cyclic$multiple_() {
	return A2(
		$elm$parser$Parser$map,
		function (children) {
			if (children.b && (!children.b.b)) {
				var x = children.a;
				return x;
			} else {
				return $author$project$Math$BinaryNode(
					{at: true, S: children, ax: true, m: '*', J: 0});
			}
		},
		A2(
			$elm$parser$Parser$loop,
			_List_Nil,
			function (list) {
				return $elm$core$List$isEmpty(list) ? A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed(
						A2($elm$core$Basics$composeR, $elm$core$List$singleton, $elm$parser$Parser$Loop)),
					A2(
						$elm$parser$Parser$ignorer,
						$author$project$Math$cyclic$negatable_(),
						$elm$parser$Parser$spaces)) : $elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									function (elem) {
										return $elm$parser$Parser$Loop(
											A2($elm$core$List$cons, elem, list));
									}),
								$elm$parser$Parser$symbol('*')),
							A2(
								$elm$parser$Parser$ignorer,
								$author$project$Math$cyclic$negatable_(),
								$elm$parser$Parser$spaces)),
							A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$ignorer,
								$elm$parser$Parser$succeed(
									function (elem) {
										return $elm$parser$Parser$Loop(
											A2(
												$elm$core$List$cons,
												$author$project$Math$UnaryNode(
													{aw: elem, m: '/', J: 0}),
												list));
									}),
								$elm$parser$Parser$symbol('/')),
							A2(
								$elm$parser$Parser$ignorer,
								$author$project$Math$cyclic$negatable_(),
								$elm$parser$Parser$spaces)),
							A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(
								function (elem) {
									return $elm$parser$Parser$Loop(
										A2($elm$core$List$cons, elem, list));
								}),
							A2(
								$elm$parser$Parser$ignorer,
								$author$project$Math$cyclic$term_(),
								$elm$parser$Parser$spaces)),
							A2(
							$elm$parser$Parser$map,
							function (_v0) {
								return $elm$parser$Parser$Done(
									$elm$core$List$reverse(list));
							},
							$elm$parser$Parser$succeed(0))
						]));
			}));
}
var $author$project$Math$negatable_ = $author$project$Math$cyclic$negatable_();
$author$project$Math$cyclic$negatable_ = function () {
	return $author$project$Math$negatable_;
};
var $author$project$Math$term_ = $author$project$Math$cyclic$term_();
$author$project$Math$cyclic$term_ = function () {
	return $author$project$Math$term_;
};
var $author$project$Math$varOrFunc_ = $author$project$Math$cyclic$varOrFunc_();
$author$project$Math$cyclic$varOrFunc_ = function () {
	return $author$project$Math$varOrFunc_;
};
var $author$project$Math$argsList_ = $author$project$Math$cyclic$argsList_();
$author$project$Math$cyclic$argsList_ = function () {
	return $author$project$Math$argsList_;
};
var $author$project$Math$expression_ = $author$project$Math$cyclic$expression_();
$author$project$Math$cyclic$expression_ = function () {
	return $author$project$Math$expression_;
};
var $author$project$Math$multiple_ = $author$project$Math$cyclic$multiple_();
$author$project$Math$cyclic$multiple_ = function () {
	return $author$project$Math$multiple_;
};
var $author$project$Math$equation_ = A2(
	$elm$parser$Parser$map,
	function (children) {
		if (children.b && (!children.b.b)) {
			var x = children.a;
			return x;
		} else {
			return $author$project$Math$BinaryNode(
				{at: true, S: children, ax: true, m: '=', J: 0});
		}
	},
	A2(
		$elm$parser$Parser$loop,
		_List_Nil,
		function (list) {
			return $elm$core$List$isEmpty(list) ? A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						A2($elm$core$Basics$composeR, $elm$core$List$singleton, $elm$parser$Parser$Loop)),
					$elm$parser$Parser$spaces),
				A2($elm$parser$Parser$ignorer, $author$project$Math$expression_, $elm$parser$Parser$spaces)) : $elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed(
								function (elem) {
									return $elm$parser$Parser$Loop(
										A2($elm$core$List$cons, elem, list));
								}),
							$elm$parser$Parser$symbol('=')),
						A2($elm$parser$Parser$ignorer, $author$project$Math$expression_, $elm$parser$Parser$spaces)),
						A2(
						$elm$parser$Parser$map,
						function (_v0) {
							return $elm$parser$Parser$Done(
								$elm$core$List$reverse(list));
						},
						$elm$parser$Parser$succeed(0))
					]));
		}));
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {bh: col, bw: problem, a_: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.a_, p.bh, p.bw);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0;
		var _v1 = parse(
			{bh: 1, c: _List_Nil, d: 1, b: 0, a_: 1, a: src});
		if (!_v1.$) {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (!_v0.$) {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $author$project$Math$parse = function (input) {
	return A2(
		$elm$core$Result$mapError,
		$author$project$Math$createErrorMessage_(input),
		A2($elm$parser$Parser$run, $author$project$Math$equation_, input));
};
var $author$project$Main$parseEquations_ = F2(
	function (elem, _v0) {
		var result = _v0.a;
		var errs = _v0.b;
		var _v1 = $author$project$Math$parse(elem);
		if (!_v1.$) {
			var root = _v1.a;
			return _Utils_Tuple2(
				_Utils_ap(
					result,
					_List_fromArray(
						[root])),
				errs);
		} else {
			var err = _v1.a;
			return _Utils_Tuple2(
				result,
				_Utils_ap(
					errs,
					_List_fromArray(
						[err])));
		}
	});
var $elm$core$String$indices = _String_indexes;
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $author$project$Query$unmarshalModel_ = F2(
	function (line, model) {
		return function (_v1) {
			var field = _v1.a;
			var value = _v1.b;
			if (field === 'eq') {
				if (!value.$) {
					var str = value.a;
					return _Utils_update(
						model,
						{
							F: _Utils_ap(
								model.F,
								_List_fromArray(
									[str]))
						});
				} else {
					return model;
				}
			} else {
				return model;
			}
		}(
			function () {
				var _v0 = A2($elm$core$String$indices, '=', line);
				if (_v0.b) {
					var x = _v0.a;
					return _Utils_Tuple2(
						A2($elm$core$String$left, x, line),
						$elm$url$Url$percentDecode(
							A2($elm$core$String$dropLeft, x + 1, line)));
				} else {
					return _Utils_Tuple2(line, $elm$core$Maybe$Nothing);
				}
			}());
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Query$parseInit = F2(
	function (url, key) {
		return A3(
			$elm$core$List$foldl,
			$author$project$Query$unmarshalModel_,
			{ae: url, F: _List_Nil, ag: key},
			A2(
				$elm$core$String$split,
				'&',
				A2($elm$core$Maybe$withDefault, '', url.ad)));
	});
var $author$project$Main$init = F3(
	function (_v0, url, key) {
		var query = A2($author$project$Query$parseInit, url, key);
		var _v1 = A3(
			$elm$core$List$foldl,
			$author$project$Main$parseEquations_,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			query.F);
		var eqs = _v1.a;
		var errs = _v1.b;
		var newScreen = $elm$core$List$isEmpty(eqs);
		var _v2 = A3(
			$elm$core$List$foldl,
			$author$project$Notification$displayError,
			_Utils_Tuple2($author$project$Notification$init, $elm$core$Platform$Cmd$none),
			errs);
		var nModel = _v2.a;
		var nCmd = _v2.b;
		return _Utils_Tuple2(
			{
				q: newScreen ? $elm$core$Maybe$Just($elm$core$Maybe$Nothing) : $elm$core$Maybe$Nothing,
				E: $elm$core$Maybe$Nothing,
				u: $author$project$Display$init(eqs),
				U: $author$project$Menu$init(
					$elm$core$Set$fromList(
						_List_fromArray(
							['Settings', 'Tutorials', 'Constants', 'Functions', 'Rules']))),
				H: nModel,
				ad: query,
				j: $author$project$Rules$init,
				A: false,
				W: newScreen ? true : false,
				P: $author$project$Tutorial$init
			},
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $author$project$Main$NotificationEvent, nCmd),
						newScreen ? $author$project$Main$focusTextBar_('textInput') : $elm$core$Platform$Cmd$none
					])));
	});
var $author$project$Main$PressedKey = function (a) {
	return {$: 8, a: a};
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$Events$Document = 0;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {aQ: pids, a4: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {aC: event, ag: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.aQ,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.ag;
		var event = _v0.aC;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.a4);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyPress = A2($elm$browser$Browser$Events$on, 0, 'keypress');
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$subscriptions = function (model) {
	var _v0 = model.q;
	if (!_v0.$) {
		return $elm$browser$Browser$Events$onKeyPress(
			A2(
				$elm$json$Json$Decode$map,
				$author$project$Main$PressedKey,
				A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)));
	} else {
		return $elm$core$Platform$Sub$none;
	}
};
var $author$project$Main$DisplayEvent = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$FileLoaded = F2(
	function (a, b) {
		return {$: 21, a: a, b: b};
	});
var $author$project$Main$FileSelected = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
	});
var $author$project$Main$ProcessTopic = F2(
	function (a, b) {
		return {$: 18, a: a, b: b};
	});
var $author$project$Main$RulesEvent = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$TutorialEvent = function (a) {
	return {$: 4, a: a};
};
var $elm$core$Set$foldl = F3(
	function (func, initialState, _v0) {
		var dict = _v0;
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (key, _v1, state) {
					return A2(func, key, state);
				}),
			initialState,
			dict);
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$String$toLower = _String_toLower;
var $author$project$Rules$deleteTopic = F2(
	function (name, model) {
		var id = $elm$core$String$toLower(name);
		var _v0 = A2($elm$core$Dict$get, id, model.C);
		if (_v0.$ === 1) {
			return model;
		} else {
			var topic = _v0.a;
			var newFunctions = A3(
				$elm$core$Dict$foldl,
				F3(
					function (n, _v2, newDict) {
						var _v3 = A2($elm$core$Dict$get, n, newDict);
						if (_v3.$ === 1) {
							return newDict;
						} else {
							var _v4 = _v3.a;
							var props = _v4.a;
							var i = _v4.b;
							return (i < 2) ? A2($elm$core$Dict$remove, n, newDict) : A3(
								$elm$core$Dict$insert,
								n,
								_Utils_Tuple2(props, i - 1),
								newDict);
						}
					}),
				model.r,
				topic.r);
			var newConstants = A3(
				$elm$core$Set$foldl,
				F2(
					function (n, newSet) {
						var _v1 = A2($elm$core$Dict$get, n, newSet);
						if (_v1.$ === 1) {
							return newSet;
						} else {
							var i = _v1.a;
							return (i < 2) ? A2($elm$core$Dict$remove, n, newSet) : A3($elm$core$Dict$insert, n, i - 1, newSet);
						}
					}),
				model.p,
				topic.p);
			return _Utils_update(
				model,
				{
					p: newConstants,
					r: newFunctions,
					C: A2($elm$core$Dict$remove, id, model.C)
				});
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $author$project$Rules$addTopic = F2(
	function (topic, m) {
		var id = $elm$core$String$toLower(topic.m);
		var model = A2($author$project$Rules$deleteTopic, id, m);
		return function (res) {
			if (res.$ === 1) {
				var errStr = res.a;
				return $elm$core$Result$Err(errStr);
			} else {
				var functions = res.a;
				return A2(
					$elm$core$Result$map,
					function (constants) {
						return _Utils_update(
							model,
							{
								p: constants,
								r: functions,
								C: A3($elm$core$Dict$insert, id, topic, model.C)
							});
					},
					A3(
						$elm$core$Set$foldl,
						F2(
							function (name, result) {
								if (result.$ === 1) {
									return result;
								} else {
									var dict = result.a;
									var _v5 = A2($elm$core$Dict$get, name, dict);
									if (_v5.$ === 1) {
										return $elm$core$Result$Ok(
											A3($elm$core$Dict$insert, name, 1, dict));
									} else {
										var count = _v5.a;
										return $elm$core$Result$Ok(
											A3($elm$core$Dict$insert, name, count + 1, dict));
									}
								}
							}),
						$elm$core$Result$Ok(model.p),
						topic.p));
			}
		}(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (name, props, result) {
						if (result.$ === 1) {
							return result;
						} else {
							var dict = result.a;
							var _v1 = A2($elm$core$Dict$get, name, dict);
							if (_v1.$ === 1) {
								return $elm$core$Result$Ok(
									A3(
										$elm$core$Dict$insert,
										name,
										_Utils_Tuple2(props, 1),
										dict));
							} else {
								var _v2 = _v1.a;
								var p = _v2.a;
								var count = _v2.b;
								return (_Utils_eq(p.ab, props.ab) && (_Utils_eq(p.ac, props.ac) && (_Utils_eq(p.at, props.at) && _Utils_eq(p.ax, props.ax)))) ? $elm$core$Result$Ok(
									A3(
										$elm$core$Dict$insert,
										name,
										_Utils_Tuple2(p, count + 1),
										dict)) : $elm$core$Result$Err('\'' + (name + '\' differs from existing definition from other topics'));
							}
						}
					}),
				$elm$core$Result$Ok(model.r),
				topic.r));
	});
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.bA));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {aW: reqs, a4: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.a6;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.aW));
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.a4)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					bd: r.bd,
					bf: r.bf,
					bl: A2(_Http_mapExpect, func, r.bl),
					aG: r.aG,
					br: r.br,
					bD: r.bD,
					a6: r.a6,
					bG: r.bG
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{bd: false, bf: r.bf, bl: r.bl, aG: r.aG, br: r.br, bD: r.bD, a6: r.a6, bG: r.bG}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{bf: $elm$http$Http$emptyBody, bl: r.bl, aG: _List_Nil, br: 'GET', bD: $elm$core$Maybe$Nothing, a6: $elm$core$Maybe$Nothing, bG: r.bG});
};
var $author$project$Main$httpErrorToString_ = F2(
	function (url, err) {
		switch (err.$) {
			case 0:
				return 'Invalid URL provided: ' + url;
			case 1:
				return 'Timed out waitiing for: ' + url;
			case 2:
				return 'Unable to reach: ' + url;
			case 3:
				var code = err.a;
				return 'The url returned an error code [' + ($elm$core$String$fromInt(code) + (']: ' + url));
			default:
				var str = err.a;
				return 'The file is malformed:\n' + str;
		}
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Display$listEquations = function (model) {
	return A2(
		$elm$core$Dict$map,
		function (_v0) {
			return $elm$core$Tuple$second;
		},
		model.F);
};
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $author$project$Query$marshalEquations_ = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm$core$List$foldl,
		F2(
			function (line, result) {
				return A2(
					$elm$core$List$cons,
					'eq=' + $elm$url$Url$percentEncode(line),
					result);
			}),
		_List_Nil),
	function (list) {
		return A2($elm$core$String$join, '&', list);
	});
var $elm$browser$Browser$Navigation$replaceUrl = _Browser_replaceUrl;
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 1) {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 1) {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.aU;
		if (!_v0) {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.aF,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.ad,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.aR,
					_Utils_ap(http, url.aJ)),
				url.aP)));
};
var $author$project$Query$pushUrl = function (model) {
	var eqs = $author$project$Query$marshalEquations_(model.F);
	return A2(
		$elm$browser$Browser$Navigation$replaceUrl,
		model.ag,
		function (url) {
			return $elm$url$Url$toString(
				_Utils_update(
					url,
					{
						ad: $elm$core$Maybe$Just(eqs)
					}));
		}(model.ae));
};
var $author$project$Query$eqToString_ = function (root) {
	if (!root.$) {
		var str = root.a;
		return str;
	} else {
		var s = root.a;
		return A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, $author$project$Query$eqToString_, s.S));
	}
};
var $author$project$Math$Node = function (a) {
	return {$: 1, a: a};
};
var $author$project$Math$Text = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Math$functionName_ = function (node) {
	switch (node.$) {
		case 0:
			return '';
		case 1:
			return '';
		case 2:
			var s = node.a;
			return s.m;
		case 3:
			var s = node.a;
			return s.m;
		default:
			var s = node.a;
			return s.m;
	}
};
var $author$project$Math$functionPrecedence_ = function (node) {
	var _v0 = $author$project$Math$functionName_(node);
	switch (_v0) {
		case '*':
			return 2;
		case '/':
			return 2;
		case '+':
			return 1;
		case '-':
			return 1;
		case '=':
			return 0;
		default:
			return -1;
	}
};
var $author$project$Math$getState = function (node) {
	switch (node.$) {
		case 0:
			var s = node.a;
			return s.J;
		case 1:
			var s = node.a;
			return s.J;
		case 2:
			var s = node.a;
			return s.J;
		case 3:
			var s = node.a;
			return s.J;
		default:
			var s = node.a;
			return s.J;
	}
};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $author$project$Math$symbolicateRecursive_ = F3(
	function (parent, multiplicativeFirst, root) {
		return function (tokens) {
			var _v3 = _Utils_Tuple2(parent, root);
			_v3$0:
			while (true) {
				switch (_v3.b.$) {
					case 0:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							return $author$project$Math$Node(
								{
									S: tokens,
									J: $author$project$Math$getState(root)
								});
						}
					case 1:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							return $author$project$Math$Node(
								{
									S: tokens,
									J: $author$project$Math$getState(root)
								});
						}
					case 4:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							return $author$project$Math$Node(
								{
									S: tokens,
									J: $author$project$Math$getState(root)
								});
						}
					default:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							var p = _v3.a.a;
							return (_Utils_cmp(
								$author$project$Math$functionPrecedence_(p),
								$author$project$Math$functionPrecedence_(root)) > 0) ? $author$project$Math$Node(
								{
									S: A2(
										$elm$core$List$cons,
										$author$project$Math$Text('('),
										_Utils_ap(
											tokens,
											_List_fromArray(
												[
													$author$project$Math$Text(')')
												]))),
									J: $author$project$Math$getState(root)
								}) : $author$project$Math$Node(
								{
									S: tokens,
									J: $author$project$Math$getState(root)
								});
						}
				}
			}
			var _v4 = _v3.a;
			return $author$project$Math$Node(
				{
					S: tokens,
					J: $author$project$Math$getState(root)
				});
		}(
			function () {
				switch (root.$) {
					case 0:
						var s = root.a;
						return _List_fromArray(
							[
								$author$project$Math$Text(
								$elm$core$String$fromFloat(s.ar))
							]);
					case 1:
						var s = root.a;
						return ($elm$core$String$length(s.m) === 1) ? _List_fromArray(
							[
								$author$project$Math$Text(s.m)
							]) : _List_fromArray(
							[
								$author$project$Math$Text('\\' + s.m)
							]);
					case 2:
						var s = root.a;
						return (s.m === '/') ? (multiplicativeFirst ? _List_fromArray(
							[
								$author$project$Math$Text('1/'),
								A3(
								$author$project$Math$symbolicateRecursive_,
								$elm$core$Maybe$Just(root),
								false,
								s.aw)
							]) : _List_fromArray(
							[
								$author$project$Math$Text('/'),
								A3(
								$author$project$Math$symbolicateRecursive_,
								$elm$core$Maybe$Just(root),
								false,
								s.aw)
							])) : _List_fromArray(
							[
								$author$project$Math$Text(s.m),
								A3(
								$author$project$Math$symbolicateRecursive_,
								$elm$core$Maybe$Just(root),
								true,
								s.aw)
							]);
					case 3:
						var s = root.a;
						return A3(
							$elm$core$List$foldl,
							F2(
								function (child, result) {
									if ($elm$core$List$isEmpty(result)) {
										return _List_fromArray(
											[
												A3(
												$author$project$Math$symbolicateRecursive_,
												$elm$core$Maybe$Just(root),
												true,
												child)
											]);
									} else {
										if (child.$ === 2) {
											var c = child.a;
											var _v2 = _Utils_Tuple2(s.m, c.m);
											_v2$2:
											while (true) {
												switch (_v2.a) {
													case '+':
														if (_v2.b === '-') {
															return _Utils_ap(
																result,
																_List_fromArray(
																	[
																		A3(
																		$author$project$Math$symbolicateRecursive_,
																		$elm$core$Maybe$Just(root),
																		true,
																		child)
																	]));
														} else {
															break _v2$2;
														}
													case '*':
														if (_v2.b === '/') {
															return _Utils_ap(
																result,
																_List_fromArray(
																	[
																		A3(
																		$author$project$Math$symbolicateRecursive_,
																		$elm$core$Maybe$Just(root),
																		$elm$core$List$isEmpty(result),
																		child)
																	]));
														} else {
															break _v2$2;
														}
													default:
														break _v2$2;
												}
											}
											return _Utils_ap(
												result,
												_List_fromArray(
													[
														$author$project$Math$Text(s.m),
														A3(
														$author$project$Math$symbolicateRecursive_,
														$elm$core$Maybe$Just(root),
														true,
														child)
													]));
										} else {
											return _Utils_ap(
												result,
												_List_fromArray(
													[
														$author$project$Math$Text(s.m),
														A3(
														$author$project$Math$symbolicateRecursive_,
														$elm$core$Maybe$Just(root),
														true,
														child)
													]));
										}
									}
								}),
							_List_Nil,
							s.S);
					default:
						var s = root.a;
						return function (_arguments) {
							return A2(
								$elm$core$List$cons,
								$author$project$Math$Text('\\' + (s.m + '(')),
								_Utils_ap(
									_arguments,
									_List_fromArray(
										[
											$author$project$Math$Text(')')
										])));
						}(
							A2(
								$elm$core$List$intersperse,
								$author$project$Math$Text(','),
								A2(
									$elm$core$List$map,
									A2(
										$author$project$Math$symbolicateRecursive_,
										$elm$core$Maybe$Just(root),
										true),
									s.S)));
				}
			}());
	});
var $author$project$Math$symbolicate = function (root) {
	return A3($author$project$Math$symbolicateRecursive_, $elm$core$Maybe$Nothing, true, root);
};
var $author$project$Query$setEquations = F2(
	function (dict, model) {
		return function (result) {
			return _Utils_update(
				model,
				{F: result});
		}(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (_v0, elem, result) {
						return function (symbols) {
							return A2(
								$elm$core$List$cons,
								$author$project$Query$eqToString_(symbols),
								result);
						}(
							$author$project$Math$symbolicate(elem));
					}),
				_List_Nil,
				dict));
	});
var $author$project$Main$submitNotification_ = F2(
	function (model, str) {
		var _v0 = A2(
			$author$project$Notification$displayError,
			str,
			_Utils_Tuple2(model.H, $elm$core$Platform$Cmd$none));
		var nModel = _v0.a;
		var nCmd = _v0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{H: nModel}),
			A2($elm$core$Platform$Cmd$map, $author$project$Main$NotificationEvent, nCmd));
	});
var $elm$file$File$toString = _File_toString;
var $author$project$Rules$Topic = F4(
	function (name, constants, functions, rules) {
		return {p: constants, r: functions, m: name, j: rules};
	});
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $author$project$Rules$FunctionProperties_ = F4(
	function (args, params, associative, commutative) {
		return {ab: args, at: associative, ax: commutative, ac: params};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Rules$functionDecoder_ = $elm$json$Json$Decode$dict(
	A5(
		$elm$json$Json$Decode$map4,
		$author$project$Rules$FunctionProperties_,
		A2($elm$json$Json$Decode$field, 'arguments', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'parameters', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'associative', $elm$json$Json$Decode$bool),
		A2($elm$json$Json$Decode$field, 'commutative', $elm$json$Json$Decode$bool)));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$Rules$Rule_ = F6(
	function (title, description, reversible, lhs, rhs, parameters) {
		return {aA: description, ah: lhs, bv: parameters, am: reversible, an: rhs, bE: title};
	});
var $author$project$Rules$extractUniqueTokens_ = F3(
	function (functions, constants, parameters) {
		return A2(
			$elm$core$Dict$foldl,
			F3(
				function (name, token, result) {
					if (result.$ === 1) {
						return result;
					} else {
						var dict = result.a;
						if (A2($elm$core$Dict$member, name, functions)) {
							if (token.$ === 1) {
								return $elm$core$Result$Err('\'' + (name + '\' is use as a variable, even though it is defined as a function'));
							} else {
								return $elm$core$Result$Ok(dict);
							}
						} else {
							if (A2($elm$core$Set$member, name, constants)) {
								if (!token.$) {
									return $elm$core$Result$Err('\'' + (name + '\' is used as a function, even though it is defined as a constant'));
								} else {
									return $elm$core$Result$Ok(dict);
								}
							} else {
								var _v3 = _Utils_Tuple2(
									A2($elm$core$Dict$get, name, parameters),
									token);
								_v3$3:
								while (true) {
									if (_v3.a.$ === 1) {
										var _v4 = _v3.a;
										return $elm$core$Result$Ok(
											A3($elm$core$Dict$insert, name, token, dict));
									} else {
										if (_v3.a.a.$ === 1) {
											if (_v3.b.$ === 1) {
												var _v5 = _v3.a.a;
												var _v6 = _v3.b;
												return $elm$core$Result$Ok(dict);
											} else {
												break _v3$3;
											}
										} else {
											if (!_v3.b.$) {
												var _v7 = _v3.a.a;
												var tArgs = _v7.a;
												var tParams = _v7.b;
												var _v8 = _v3.b;
												var args = _v8.a;
												var params = _v8.b;
												return (_Utils_eq(tArgs, args) && _Utils_eq(tParams, params)) ? $elm$core$Result$Ok(dict) : $elm$core$Result$Err('Inconsistent function signature \'' + (name + '\' between expression and parameter'));
											} else {
												break _v3$3;
											}
										}
									}
								}
								return $elm$core$Result$Err('Inconsistent usage of \'' + (name + '\' between expression and parameter'));
							}
						}
					}
				}),
			$elm$core$Result$Ok($elm$core$Dict$empty));
	});
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$map6 = _Json_map6;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Rules$parseExpression_ = F2(
	function (functions, str) {
		var _v0 = $author$project$Math$parse(str);
		if (_v0.$ === 1) {
			var errStr = _v0.a;
			return $elm$core$Result$Err('\'' + (str + ('\' is not a valid expression: ' + errStr)));
		} else {
			var root = _v0.a;
			return $elm$core$Result$Ok(
				{m: str, aZ: root, Z: $elm$core$Dict$empty});
		}
	});
var $author$project$Rules$resultToParser_ = function (res) {
	if (res.$ === 1) {
		var str = res.a;
		return $elm$json$Json$Decode$fail(str);
	} else {
		var b = res.a;
		return $elm$json$Json$Decode$succeed(b);
	}
};
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$union, dict1, dict2);
	});
var $author$project$Rules$ruleDecoder_ = F2(
	function (functions, constants) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (_v12) {
				var _v13 = _v12.a;
				var title = _v13.a;
				var description = _v13.b;
				var reversible = _v13.c;
				var _v14 = _v12.b;
				var originalLhs = _v14.a;
				var originalRhs = _v14.b;
				var _v15 = _v14.c;
				var parameters = _v15.a;
				var pDict = _v15.b;
				var _v16 = A4($author$project$Rules$extractUniqueTokens_, functions, constants, pDict, originalLhs.Z);
				if (_v16.$ === 1) {
					var errStr = _v16.a;
					return $elm$json$Json$Decode$fail(errStr);
				} else {
					var lhs = _v16.a;
					var _v17 = A4($author$project$Rules$extractUniqueTokens_, functions, constants, pDict, originalRhs.Z);
					if (_v17.$ === 1) {
						var errStr = _v17.a;
						return $elm$json$Json$Decode$fail(errStr);
					} else {
						var rhs = _v17.a;
						return $author$project$Rules$resultToParser_(
							A2(
								$elm$core$Result$map,
								function (_v28) {
									return A6($author$project$Rules$Rule_, title, description, reversible, originalLhs, originalRhs, parameters);
								},
								A3(
									$elm$core$Set$foldl,
									F2(
										function (name, result) {
											if (result.$ === 1) {
												return result;
											} else {
												var _v19 = _Utils_Tuple2(
													A2($elm$core$Dict$get, name, lhs),
													A2($elm$core$Dict$get, name, rhs));
												_v19$5:
												while (true) {
													if (_v19.a.$ === 1) {
														if (_v19.b.$ === 1) {
															var _v20 = _v19.a;
															var _v21 = _v19.b;
															return $elm$core$Result$Err('Something went wrong, unable to find name \'' + (name + '\'in lhs nor rhs'));
														} else {
															var _v22 = _v19.a;
															return $elm$core$Result$Ok(0);
														}
													} else {
														if (_v19.b.$ === 1) {
															var _v23 = _v19.b;
															return $elm$core$Result$Ok(0);
														} else {
															if (_v19.a.a.$ === 1) {
																if (_v19.b.a.$ === 1) {
																	var _v24 = _v19.a.a;
																	var _v25 = _v19.b.a;
																	return $elm$core$Result$Ok(0);
																} else {
																	break _v19$5;
																}
															} else {
																if (!_v19.b.a.$) {
																	var _v26 = _v19.a.a;
																	var lArgs = _v26.a;
																	var lParam = _v26.b;
																	var _v27 = _v19.b.a;
																	var rArgs = _v27.a;
																	var rParam = _v27.b;
																	return (_Utils_eq(lArgs, rArgs) && _Utils_eq(lParam, rParam)) ? $elm$core$Result$Ok(0) : $elm$core$Result$Err('inconsistent function signature for \'' + (name + '\' in lhs and rhs'));
																} else {
																	break _v19$5;
																}
															}
														}
													}
												}
												return $elm$core$Result$Err('lhs and rhs are treating \'' + (name + '\' differently'));
											}
										}),
									$elm$core$Result$Ok(0),
									A2(
										$elm$core$Set$union,
										$elm$core$Set$fromList(
											$elm$core$Dict$keys(lhs)),
										$elm$core$Set$fromList(
											$elm$core$Dict$keys(rhs))))));
					}
				}
			},
			A7(
				$elm$json$Json$Decode$map6,
				F6(
					function (a, b, c, d, e, f) {
						return _Utils_Tuple2(
							_Utils_Tuple3(a, b, c),
							_Utils_Tuple3(d, e, f));
					}),
				A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'description', $elm$json$Json$Decode$string),
				A2(
					$elm$json$Json$Decode$map,
					$elm$core$Maybe$withDefault(false),
					$elm$json$Json$Decode$maybe(
						A2($elm$json$Json$Decode$field, 'reversible', $elm$json$Json$Decode$bool))),
				A2(
					$elm$json$Json$Decode$field,
					'lhs',
					A2(
						$elm$json$Json$Decode$andThen,
						A2(
							$elm$core$Basics$composeR,
							$author$project$Rules$parseExpression_(functions),
							$author$project$Rules$resultToParser_),
						$elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$field,
					'rhs',
					A2(
						$elm$json$Json$Decode$andThen,
						A2(
							$elm$core$Basics$composeR,
							$author$project$Rules$parseExpression_(functions),
							$author$project$Rules$resultToParser_),
						$elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$map,
					$elm$core$Maybe$withDefault(
						_Utils_Tuple2(_List_Nil, $elm$core$Dict$empty)),
					$elm$json$Json$Decode$maybe(
						A2(
							$elm$json$Json$Decode$field,
							'parameters',
							A2(
								$elm$json$Json$Decode$andThen,
								function (list) {
									return $author$project$Rules$resultToParser_(
										A3(
											$elm$core$List$foldl,
											F2(
												function (_v0, res) {
													var key = _v0.a;
													var description = _v0.b;
													if (res.$ === 1) {
														return res;
													} else {
														var _v2 = res.a;
														var others = _v2.a;
														var dict = _v2.b;
														var _v3 = A2($author$project$Rules$parseExpression_, functions, key);
														if (_v3.$ === 1) {
															var str = _v3.a;
															return $elm$core$Result$Err(str);
														} else {
															var exp = _v3.a;
															return function (result) {
																if (result.$ === 1) {
																	var errStr = result.a;
																	return $elm$core$Result$Err(errStr);
																} else {
																	var newDict = result.a;
																	return $elm$core$Result$Ok(
																		_Utils_Tuple2(
																			_Utils_ap(
																				others,
																				_List_fromArray(
																					[
																						_Utils_Tuple2(exp, description)
																					])),
																			newDict));
																}
															}(
																A3(
																	$elm$core$Dict$foldl,
																	F3(
																		function (name, tok, result) {
																			if (result.$ === 1) {
																				return result;
																			} else {
																				var newDict = result.a;
																				var _v5 = A2($elm$core$Dict$get, name, newDict);
																				if (_v5.$ === 1) {
																					return $elm$core$Result$Ok(
																						A3($elm$core$Dict$insert, name, tok, newDict));
																				} else {
																					var oldTok = _v5.a;
																					var _v6 = _Utils_Tuple2(oldTok, tok);
																					_v6$2:
																					while (true) {
																						if (!_v6.a.$) {
																							if (!_v6.b.$) {
																								var _v7 = _v6.a;
																								var oArgs = _v7.a;
																								var oNum = _v7.b;
																								var _v8 = _v6.b;
																								var args = _v8.a;
																								var num = _v8.b;
																								return (_Utils_eq(oArgs, args) && _Utils_eq(oNum, num)) ? $elm$core$Result$Ok(newDict) : $elm$core$Result$Err('Different function signature for \'' + (name + '\' detected within parameters'));
																							} else {
																								break _v6$2;
																							}
																						} else {
																							if (_v6.b.$ === 1) {
																								var _v9 = _v6.a;
																								var _v10 = _v6.b;
																								return $elm$core$Result$Ok(newDict);
																							} else {
																								break _v6$2;
																							}
																						}
																					}
																					return $elm$core$Result$Err('Different usages of \'' + (name + '\' detected within parameters'));
																				}
																			}
																		}),
																	$elm$core$Result$Ok(dict),
																	exp.Z));
														}
													}
												}),
											$elm$core$Result$Ok(
												_Utils_Tuple2(_List_Nil, $elm$core$Dict$empty)),
											list));
								},
								$elm$json$Json$Decode$keyValuePairs($elm$json$Json$Decode$string)))))));
	});
var $author$project$Rules$topicDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (_v0) {
		var name = _v0.a;
		var functions = _v0.b;
		var variables = _v0.c;
		var vars = $elm$core$Set$fromList(variables);
		return A2(
			$elm$json$Json$Decode$map,
			function (eqs) {
				return A4($author$project$Rules$Topic, name, vars, functions, eqs);
			},
			A2(
				$elm$json$Json$Decode$field,
				'equations',
				$elm$json$Json$Decode$list(
					A2($author$project$Rules$ruleDecoder_, functions, vars))));
	},
	A4(
		$elm$json$Json$Decode$map3,
		F3(
			function (a, b, c) {
				return _Utils_Tuple3(a, b, c);
			}),
		A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'functions', $author$project$Rules$functionDecoder_),
		A2(
			$elm$json$Json$Decode$field,
			'constants',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))));
var $author$project$Display$update = F2(
	function (event, model) {
		switch (event.$) {
			case 0:
				var eq = event.a;
				var node = event.b;
				var _v1 = model.x;
				if (_v1.$ === 1) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								x: $elm$core$Maybe$Just(
									_Utils_Tuple2(eq, node))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var _v2 = _v1.a;
					var e = _v2.a;
					var n = _v2.b;
					return (_Utils_eq(e, eq) && _Utils_eq(n, node)) ? _Utils_Tuple2(
						_Utils_update(
							model,
							{x: $elm$core$Maybe$Nothing}),
						$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								x: $elm$core$Maybe$Just(
									_Utils_Tuple2(eq, node))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 1:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{x: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$remove, key, dict);
	});
var $author$project$Menu$update = F2(
	function (event, model) {
		var entry = event;
		return A2($elm$core$Set$member, entry, model.B) ? _Utils_update(
			model,
			{
				B: A2($elm$core$Set$remove, entry, model.B)
			}) : _Utils_update(
			model,
			{
				B: A2($elm$core$Set$insert, entry, model.B)
			});
	});
var $author$project$Notification$DeleteEvent = function (a) {
	return {$: 1, a: a};
};
var $author$project$Notification$delayedDelete_ = function (id) {
	return A2(
		$elm$core$Task$perform,
		function (_v0) {
			return $author$project$Notification$DeleteEvent(id);
		},
		$elm$core$Process$sleep(750));
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Notification$update = F2(
	function (e, model) {
		if (!e.$) {
			var id = e.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						w: A3(
							$elm$core$Dict$update,
							id,
							$elm$core$Maybe$map(
								function (_v1) {
									var str = _v1.b;
									return _Utils_Tuple2(true, str);
								}),
							model.w)
					}),
				$author$project$Notification$delayedDelete_(id));
		} else {
			var id = e.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						w: A2($elm$core$Dict$remove, id, model.w)
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Rules$update = F2(
	function (_v0, model) {
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Tutorial$update = F2(
	function (_v0, model) {
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Display$updateEquation = F3(
	function (_v0, _v1, model) {
		return model;
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Main$updateMathJax = _Platform_outgoingPort(
	'updateMathJax',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Main$update = F2(
	function (event, model) {
		switch (event.$) {
			case 0:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 1:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 2:
				var e = event.a;
				var _v1 = A2($author$project$Display$update, e, model.u);
				var dModel = _v1.a;
				var dCmd = _v1.b;
				var query = A2(
					$author$project$Query$setEquations,
					$author$project$Display$listEquations(dModel),
					model.ad);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{u: dModel}),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2($elm$core$Platform$Cmd$map, $author$project$Main$DisplayEvent, dCmd),
								$author$project$Query$pushUrl(query),
								$author$project$Main$updateMathJax(0)
							])));
			case 3:
				var e = event.a;
				var _v2 = A2($author$project$Rules$update, e, model.j);
				var rModel = _v2.a;
				var rCmd = _v2.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{j: rModel}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$RulesEvent, rCmd));
			case 4:
				var e = event.a;
				var _v3 = A2($author$project$Tutorial$update, e, model.P);
				var tModel = _v3.a;
				var tCmd = _v3.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{P: tModel}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$TutorialEvent, tCmd));
			case 5:
				var e = event.a;
				var _v4 = A2($author$project$Notification$update, e, model.H);
				var nModel = _v4.a;
				var nCmd = _v4.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{H: nModel}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$NotificationEvent, nCmd));
			case 6:
				var e = event.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							U: A2($author$project$Menu$update, e, model.U)
						}),
					$elm$core$Platform$Cmd$none);
			case 7:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 8:
				var str = event.a;
				return (str === 'Escape') ? _Utils_Tuple2(
					_Utils_update(
						model,
						{q: $elm$core$Maybe$Nothing, A: false}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 9:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							q: $elm$core$Maybe$Just($elm$core$Maybe$Nothing)
						}),
					$author$project$Main$focusTextBar_('textInput'));
			case 10:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{q: $elm$core$Maybe$Nothing, A: false}),
					$elm$core$Platform$Cmd$none);
			case 11:
				var id = event.a;
				var str = event.b;
				var _v5 = $author$project$Math$parse(str);
				if (!_v5.$) {
					var root = _v5.a;
					return function (dModel) {
						return function (query) {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{q: $elm$core$Maybe$Nothing, u: dModel, A: false}),
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											$author$project$Query$pushUrl(query),
											$author$project$Main$updateMathJax(0)
										])));
						}(
							function (list) {
								return A2($author$project$Query$setEquations, list, model.ad);
							}(
								$author$project$Display$listEquations(dModel)));
					}(
						function () {
							if (id.$ === 1) {
								return A2($author$project$Display$addEquation, root, model.u);
							} else {
								var i = id.a;
								return A3($author$project$Display$updateEquation, i, root, model.u);
							}
						}());
				} else {
					var err = _v5.a;
					return A2($author$project$Main$submitNotification_, model, err);
				}
			case 12:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{A: !model.A}),
					$author$project$Main$focusTextBar_('textInput'));
			case 13:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{W: !model.W}),
					$elm$core$Platform$Cmd$none);
			case 14:
				var title = event.a;
				var generator = event.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							E: $elm$core$Maybe$Just(
								_Utils_Tuple2(title, generator))
						}),
					$author$project$Main$focusTextBar_('dialogAnswer'));
			case 15:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{E: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 16:
				var name = event.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							E: $elm$core$Maybe$Nothing,
							j: A2($author$project$Rules$deleteTopic, name, model.j)
						}),
					$elm$core$Platform$Cmd$none);
			case 17:
				var url = event.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{E: $elm$core$Maybe$Nothing}),
					$elm$http$Http$get(
						{
							bl: A2(
								$elm$http$Http$expectJson,
								$author$project$Main$ProcessTopic(url),
								$author$project$Rules$topicDecoder),
							bG: url
						}));
			case 18:
				var url = event.a;
				var result = event.b;
				if (result.$ === 1) {
					var err = result.a;
					return A2(
						$author$project$Main$submitNotification_,
						model,
						A2($author$project$Main$httpErrorToString_, url, err));
				} else {
					var topic = result.a;
					var _v8 = A2($author$project$Rules$addTopic, topic, model.j);
					if (_v8.$ === 1) {
						var errStr = _v8.a;
						return A2($author$project$Main$submitNotification_, model, errStr);
					} else {
						var rModel = _v8.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{j: rModel}),
							$elm$core$Platform$Cmd$none);
					}
				}
			case 19:
				var fileType = event.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['application/json']),
						$author$project$Main$FileSelected(fileType)));
			case 20:
				var fileType = event.a;
				var file = event.b;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$core$Task$perform,
						$author$project$Main$FileLoaded(fileType),
						$elm$file$File$toString(file)));
			default:
				var fileType = event.a;
				var str = event.b;
				if (!fileType) {
					return function (result) {
						if (result.$ === 1) {
							var errStr = result.a;
							return A2($author$project$Main$submitNotification_, model, errStr);
						} else {
							var rModel = result.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{j: rModel}),
								$elm$core$Platform$Cmd$none);
						}
					}(
						A2(
							$elm$core$Result$andThen,
							function (topic) {
								return A2($author$project$Rules$addTopic, topic, model.j);
							},
							A2(
								$elm$core$Result$mapError,
								$elm$json$Json$Decode$errorToString,
								A2($elm$json$Json$Decode$decodeString, $author$project$Rules$topicDecoder, str))));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
		}
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Menu$Content = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$DeleteTopic = function (a) {
	return {$: 16, a: a};
};
var $author$project$Main$DownloadTopic = function (a) {
	return {$: 17, a: a};
};
var $author$project$Main$FileSelect = function (a) {
	return {$: 19, a: a};
};
var $author$project$Main$MenuEvent = function (a) {
	return {$: 6, a: a};
};
var $author$project$Main$OpenDialog = F2(
	function (a, b) {
		return {$: 14, a: a, b: b};
	});
var $author$project$Menu$Section = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$Main$ToggleMenu = {$: 13};
var $author$project$Main$TopicFile = 0;
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $author$project$Icon$class = $elm$svg$Svg$Attributes$class;
var $author$project$Main$CloseDialog = {$: 15};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$mask = $elm$svg$Svg$trustedNode('mask');
var $elm$svg$Svg$Attributes$mask = _VirtualDom_attribute('mask');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Icon$cancel = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		_Utils_ap(
			attr,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$viewBox('0 0 24 24')
				])),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$defs,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$mask,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id('cancelMask')
							]),
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$rect,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$width('100%'),
										$elm$svg$Svg$Attributes$height('100%'),
										$elm$svg$Svg$Attributes$fill('#fff'),
										$elm$svg$Svg$Attributes$x('0'),
										$elm$svg$Svg$Attributes$y('0')
									]),
								_List_Nil),
								A2(
								$elm$svg$Svg$path,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$d('M6 6L18 18M6 18L18 6'),
										$elm$svg$Svg$Attributes$fill('none'),
										$elm$svg$Svg$Attributes$stroke('#000'),
										$elm$svg$Svg$Attributes$strokeWidth('2')
									]),
								_List_Nil)
							]))
					])),
				A2(
				$elm$svg$Svg$circle,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$r('12'),
						$elm$svg$Svg$Attributes$cx('12'),
						$elm$svg$Svg$Attributes$cy('12'),
						$elm$svg$Svg$Attributes$stroke('none'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$mask('url(#cancelMask)')
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$form = _VirtualDom_node('form');
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $author$project$HtmlEvent$onClick = function (event) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'click',
		$elm$json$Json$Decode$succeed(
			_Utils_Tuple2(event, true)));
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $author$project$HtmlEvent$onSubmitField = F2(
	function (target, event) {
		return A2(
			$elm$html$Html$Events$preventDefaultOn,
			'submit',
			A2(
				$elm$json$Json$Decode$map,
				function (input) {
					return _Utils_Tuple2(
						event(input),
						true);
				},
				A2(
					$elm$json$Json$Decode$field,
					'target',
					A2(
						$elm$json$Json$Decode$field,
						target,
						A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string)))));
	});
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Icon$tick = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		_Utils_ap(
			attr,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$viewBox('0 0 24 24')
				])),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$defs,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$mask,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id('tickMask')
							]),
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$rect,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$width('100%'),
										$elm$svg$Svg$Attributes$height('100%'),
										$elm$svg$Svg$Attributes$fill('#fff'),
										$elm$svg$Svg$Attributes$x('0'),
										$elm$svg$Svg$Attributes$y('0')
									]),
								_List_Nil),
								A2(
								$elm$svg$Svg$path,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$d('M6 15L11 19L17 6'),
										$elm$svg$Svg$Attributes$fill('none'),
										$elm$svg$Svg$Attributes$stroke('#000'),
										$elm$svg$Svg$Attributes$strokeWidth('2')
									]),
								_List_Nil)
							]))
					])),
				A2(
				$elm$svg$Svg$circle,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$r('12'),
						$elm$svg$Svg$Attributes$cx('12'),
						$elm$svg$Svg$Attributes$cy('12'),
						$elm$svg$Svg$Attributes$stroke('none'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$mask('url(#tickMask)')
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$dialogPane_ = function (selection) {
	if (selection.$ === 1) {
		return _List_Nil;
	} else {
		var _v1 = selection.a;
		var prompt = _v1.a;
		var event = _v1.b;
		return _List_fromArray(
			[
				A3(
				$elm$html$Html$node,
				'dialog',
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'open', 'true')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$form,
						_List_fromArray(
							[
								A2($author$project$HtmlEvent$onSubmitField, 'answer', event),
								A2($elm$html$Html$Attributes$attribute, 'method', 'dialog')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(prompt + ':'),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('text'),
										$elm$html$Html$Attributes$name('answer'),
										$elm$html$Html$Attributes$id('dialogAnswer')
									]),
								_List_Nil),
								$author$project$Icon$cancel(
								_List_fromArray(
									[
										$author$project$Icon$class('clickable'),
										$author$project$Icon$class('cancelable'),
										$author$project$HtmlEvent$onClick($author$project$Main$CloseDialog)
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('submit')
									]),
								_List_fromArray(
									[
										$author$project$Icon$tick(
										_List_fromArray(
											[
												$author$project$Icon$class('clickable'),
												$author$project$Icon$class('submitable')
											]))
									]))
							]))
					]))
			]);
	}
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Main$inCreateMode_ = function (model) {
	var _v0 = model.q;
	if (_v0.$ === 1) {
		return false;
	} else {
		return true;
	}
};
var $author$project$Main$CancelCreateMode = {$: 10};
var $author$project$Main$EnterCreateMode = {$: 9};
var $author$project$Main$SubmitEquation = F2(
	function (a, b) {
		return {$: 11, a: a, b: b};
	});
var $author$project$Main$ToggleHelp = {$: 12};
var $author$project$Icon$add = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		_Utils_ap(
			attr,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$viewBox('0 0 24 24')
				])),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$defs,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$mask,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id('addMask')
							]),
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$rect,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$width('100%'),
										$elm$svg$Svg$Attributes$height('100%'),
										$elm$svg$Svg$Attributes$fill('#fff'),
										$elm$svg$Svg$Attributes$x('0'),
										$elm$svg$Svg$Attributes$y('0')
									]),
								_List_Nil),
								A2(
								$elm$svg$Svg$path,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$d('M12 6V18M6 12H18'),
										$elm$svg$Svg$Attributes$fill('none'),
										$elm$svg$Svg$Attributes$stroke('#000'),
										$elm$svg$Svg$Attributes$strokeWidth('2')
									]),
								_List_Nil)
							]))
					])),
				A2(
				$elm$svg$Svg$circle,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$r('12'),
						$elm$svg$Svg$Attributes$cx('12'),
						$elm$svg$Svg$Attributes$cy('12'),
						$elm$svg$Svg$Attributes$stroke('none'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$mask('url(#addMask)')
					]),
				_List_Nil)
			]));
};
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $author$project$Icon$help = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		_Utils_ap(
			attr,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$viewBox('0 0 24 24')
				])),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$defs,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$mask,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id('helpMask')
							]),
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$rect,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$width('100%'),
										$elm$svg$Svg$Attributes$height('100%'),
										$elm$svg$Svg$Attributes$fill('#fff'),
										$elm$svg$Svg$Attributes$x('0'),
										$elm$svg$Svg$Attributes$y('0')
									]),
								_List_Nil),
								A2(
								$elm$svg$Svg$text_,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$x('8'),
										$elm$svg$Svg$Attributes$y('17'),
										$elm$svg$Svg$Attributes$fill('#000')
									]),
								_List_fromArray(
									[
										$elm$svg$Svg$text('?')
									]))
							]))
					])),
				A2(
				$elm$svg$Svg$circle,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$r('12'),
						$elm$svg$Svg$Attributes$cx('12'),
						$elm$svg$Svg$Attributes$cy('12'),
						$elm$svg$Svg$Attributes$stroke('none'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$mask('url(#helpMask)')
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$Events$alwaysPreventDefault = function (msg) {
	return _Utils_Tuple2(msg, true);
};
var $elm$html$Html$Events$onSubmit = function (msg) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'submit',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysPreventDefault,
			$elm$json$Json$Decode$succeed(msg)));
};
var $author$project$HtmlEvent$onSubmit = $elm$html$Html$Events$onSubmit;
var $author$project$Main$inputDiv = function (model) {
	return A2(
		$elm$html$Html$form,
		A2(
			$elm$core$List$cons,
			$elm$html$Html$Attributes$id('textbar'),
			function () {
				var _v0 = model.q;
				if (_v0.$ === 1) {
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$class('closed'),
							$author$project$HtmlEvent$onSubmit($author$project$Main$EnterCreateMode)
						]);
				} else {
					var eq = _v0.a;
					return _List_fromArray(
						[
							A2(
							$author$project$HtmlEvent$onSubmitField,
							'equation',
							$author$project$Main$SubmitEquation(eq))
						]);
				}
			}()),
		_List_fromArray(
			[
				$author$project$Icon$help(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('help'),
						$author$project$Icon$class('clickable'),
						$author$project$Icon$class('helpable'),
						$author$project$HtmlEvent$onClick($author$project$Main$ToggleHelp)
					])),
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('text'),
						$elm$html$Html$Attributes$name('equation'),
						$elm$html$Html$Attributes$id('textInput')
					]),
				_List_Nil),
				$author$project$Icon$cancel(
				_List_fromArray(
					[
						$author$project$Icon$class('clickable'),
						$author$project$Icon$class('cancelable'),
						$author$project$HtmlEvent$onClick($author$project$Main$CancelCreateMode)
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('submit')
					]),
				_List_fromArray(
					[
						function () {
						var _v1 = model.q;
						if (_v1.$ === 1) {
							return $author$project$Icon$add(
								_List_fromArray(
									[
										$author$project$Icon$class('clickable'),
										$author$project$Icon$class('submitable')
									]));
						} else {
							return $author$project$Icon$tick(
								_List_fromArray(
									[
										$author$project$Icon$class('clickable'),
										$author$project$Icon$class('submitable')
									]));
						}
					}()
					]))
			]));
};
var $author$project$Icon$menu = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		_Utils_ap(
			attr,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$viewBox('0 0 150 50')
				])),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$defs,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$mask,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$id('menuMask')
							]),
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$rect,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$width('100%'),
										$elm$svg$Svg$Attributes$height('100%'),
										$elm$svg$Svg$Attributes$fill('#fff'),
										$elm$svg$Svg$Attributes$x('0'),
										$elm$svg$Svg$Attributes$y('0')
									]),
								_List_Nil),
								A2(
								$elm$svg$Svg$path,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$d('M50 15H100M50 35H100'),
										$elm$svg$Svg$Attributes$stroke('#000'),
										$elm$svg$Svg$Attributes$strokeWidth('10'),
										$elm$svg$Svg$Attributes$fill('none')
									]),
								_List_Nil)
							]))
					])),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d('M0 50C25 50 25 0 50 0H100C125 0 125 50 150 50Z'),
						$elm$svg$Svg$Attributes$stroke('none'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$mask('url(#menuMask)')
					]),
				_List_Nil)
			]));
};
var $author$project$Tutorial$menu = F2(
	function (converter, model) {
		return A3($author$project$Menu$Section, 'Tutorials', true, _List_Nil);
	});
var $author$project$Rules$ClickVariable = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Rules$menuConstants = F2(
	function (converter, model) {
		return A3(
			$author$project$Menu$Section,
			'Constants',
			true,
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (name, _v0, result) {
						return _Utils_ap(
							result,
							_List_fromArray(
								[
									$author$project$Menu$Content(
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h2,
											_List_fromArray(
												[
													$author$project$HtmlEvent$onClick(
													converter(
														$author$project$Rules$ClickVariable(name)))
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(name)
												]))
										]))
								]));
					}),
				_List_Nil,
				model.p));
	});
var $author$project$Rules$ClickFunction = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$Rules$menuFunctions = F3(
	function (converter, model, createMode) {
		return A3(
			$author$project$Menu$Section,
			'Functions',
			true,
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (name, _v0, result) {
						var props = _v0.a;
						return _Utils_ap(
							result,
							_List_fromArray(
								[
									A3(
									$author$project$Menu$Section,
									name,
									true,
									A2(
										$elm$core$List$cons,
										$author$project$Menu$Content(
											_List_fromArray(
												[
													A2(
													$elm$html$Html$p,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															'Arguments: ' + $elm$core$String$fromInt(props.ab))
														])),
													A2(
													$elm$html$Html$p,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															'Parameters: ' + $elm$core$String$fromInt(props.ac))
														])),
													A2(
													$elm$html$Html$p,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															'Associative: ' + (props.at ? 'Yes' : 'No'))
														])),
													A2(
													$elm$html$Html$p,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															'Commutative: ' + (props.ax ? 'Yes' : 'No'))
														]))
												])),
										createMode ? _List_fromArray(
											[
												$author$project$Menu$Content(
												_List_fromArray(
													[
														A2(
														$elm$html$Html$a,
														_List_fromArray(
															[
																$author$project$HtmlEvent$onClick(
																converter(
																	$author$project$Rules$ClickFunction(name)))
															]),
														_List_fromArray(
															[
																$elm$html$Html$text('Add')
															]))
													]))
											]) : _List_Nil))
								]));
					}),
				_List_Nil,
				model.r));
	});
var $author$project$Rules$ClickRule = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Rules$matchExpressionToNode_ = F2(
	function (exp, node) {
		return true;
	});
var $author$project$Rules$matchRuleToNode_ = F2(
	function (rule, node) {
		return A2($author$project$Rules$matchExpressionToNode_, rule.ah, node) || (rule.am && A2($author$project$Rules$matchExpressionToNode_, rule.an, node));
	});
var $author$project$Rules$menuRules = F3(
	function (converter, model, selectedNode) {
		return A3(
			$author$project$Menu$Section,
			'Rules',
			true,
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (_v0, topic, result) {
						return _Utils_ap(
							result,
							_List_fromArray(
								[
									A3(
									$author$project$Menu$Section,
									topic.m,
									true,
									A3(
										$elm$core$List$foldl,
										F2(
											function (rule, _v1) {
												var index = _v1.a;
												var res = _v1.b;
												return _Utils_Tuple2(
													index + 1,
													_Utils_ap(
														res,
														_List_fromArray(
															[
																A3(
																$author$project$Menu$Section,
																_Utils_ap(
																	rule.ah.m,
																	_Utils_ap(
																		rule.am ? '↔' : '→',
																		rule.an.m)),
																A2(
																	$elm$core$Maybe$withDefault,
																	true,
																	A2(
																		$elm$core$Maybe$map,
																		$author$project$Rules$matchRuleToNode_(rule),
																		selectedNode)),
																A2(
																	$elm$core$List$cons,
																	$author$project$Menu$Content(
																		_List_fromArray(
																			[
																				A2(
																				$elm$html$Html$h2,
																				_List_Nil,
																				_List_fromArray(
																					[
																						$elm$html$Html$text(rule.bE)
																					])),
																				A2(
																				$elm$html$Html$p,
																				_List_Nil,
																				_List_fromArray(
																					[
																						$elm$html$Html$text(rule.aA)
																					]))
																			])),
																	function () {
																		if (selectedNode.$ === 1) {
																			return _List_Nil;
																		} else {
																			return _List_fromArray(
																				[
																					$author$project$Menu$Content(
																					_List_fromArray(
																						[
																							A2(
																							$elm$html$Html$a,
																							_List_fromArray(
																								[
																									$author$project$HtmlEvent$onClick(
																									converter(
																										A2($author$project$Rules$ClickRule, topic.m, index)))
																								]),
																							_List_fromArray(
																								[
																									$elm$html$Html$text('Apply')
																								]))
																						]))
																				]);
																		}
																	}()))
															])));
											}),
										_Utils_Tuple2(0, _List_Nil),
										topic.j).b)
								]));
					}),
				_List_Nil,
				model.C));
	});
var $author$project$Math$notation = '\na - a variable called "a"\nab - a variable "a" multiplied by a variable "b"\n\\ab - a variable with a long name called "ab"\n\\a(x,y;z) - a function called "a" that takes in x and y, and uses z\n';
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Math$getChildren = function (node) {
	switch (node.$) {
		case 0:
			return _List_Nil;
		case 1:
			return _List_Nil;
		case 2:
			var s = node.a;
			return _List_fromArray(
				[s.aw]);
		case 3:
			var s = node.a;
			return s.S;
		default:
			var s = node.a;
			return s.S;
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Display$processSearchRecursive_ = F2(
	function (path, children) {
		processSearchRecursive_:
		while (true) {
			if (!children.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var root = children.a;
				var other = children.b;
				var s = $author$project$Math$getState(root);
				if (!path.b) {
					return $elm$core$Maybe$Nothing;
				} else {
					if (!path.b.b) {
						var x = path.a;
						return (!_Utils_eq(x, s.o)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(root);
					} else {
						var x = path.a;
						var next = path.b;
						if (!_Utils_eq(x, s.o)) {
							var $temp$path = path,
								$temp$children = other;
							path = $temp$path;
							children = $temp$children;
							continue processSearchRecursive_;
						} else {
							return A2(
								$author$project$Display$processSearchRecursive_,
								next,
								$author$project$Math$getChildren(root));
						}
					}
				}
			}
		}
	});
var $author$project$Display$searchPath_ = F2(
	function (_v0, id) {
		var num = _v0.a;
		var map = _v0.b;
		var _v1 = A2($elm$core$Dict$get, id, map);
		if (_v1.$ === 1) {
			return _List_Nil;
		} else {
			var a = _v1.a;
			return (a < 0) ? _List_Nil : function (list) {
				return A2($elm$core$List$cons, id, list);
			}(
				A2(
					$author$project$Display$searchPath_,
					_Utils_Tuple2(num, map),
					a));
		}
	});
var $author$project$Display$processSearch_ = F2(
	function (id, _v0) {
		var map = _v0.a;
		var root = _v0.b;
		if (!id) {
			return $elm$core$Maybe$Just(root);
		} else {
			var path = A2($author$project$Display$searchPath_, map, id);
			return A2(
				$author$project$Display$processSearchRecursive_,
				path,
				$author$project$Math$getChildren(root));
		}
	});
var $author$project$Display$selectedNode = function (model) {
	return A2(
		$elm$core$Maybe$andThen,
		function (_v0) {
			var eq = _v0.a;
			var num = _v0.b;
			return A2(
				$elm$core$Maybe$andThen,
				$author$project$Display$processSearch_(num),
				A2($elm$core$Dict$get, eq, model.F));
		},
		model.x);
};
var $author$project$Display$Select = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Display$collapsedView_ = F3(
	function (eq, highlight, node) {
		if (!node.$) {
			var val = node.a;
			return $elm$html$Html$text(val);
		} else {
			var s = node.a;
			return A2(
				$elm$html$Html$div,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('node'),
							$author$project$HtmlEvent$onClick(
							A2($author$project$Display$Select, eq, s.J.o))
						]),
					_Utils_eq(
						s.J.o,
						A2($elm$core$Maybe$withDefault, -1, highlight)) ? _List_fromArray(
						[
							$elm$html$Html$Attributes$class('selected')
						]) : _List_Nil),
				A2(
					$elm$core$List$map,
					A2($author$project$Display$collapsedView_, eq, highlight),
					s.S));
		}
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Display$stackRecursive = F5(
	function (eq, highlight, width, depth, node) {
		var state = $author$project$Math$getState(node);
		var children = $author$project$Math$getChildren(node);
		var _v0 = $elm$core$List$isEmpty(children) ? _Utils_Tuple3(width + 1, depth, _List_Nil) : A3(
			$elm$core$List$foldl,
			F2(
				function (child, _v1) {
					var foldWidth = _v1.a;
					var foldDepth = _v1.b;
					var foldDivs = _v1.c;
					var _v2 = A5($author$project$Display$stackRecursive, eq, highlight, foldWidth, depth + 1, child);
					var w = _v2.a;
					var d = _v2.b;
					var divs = _v2.c;
					return _Utils_Tuple3(
						w,
						A2($elm$core$Basics$max, foldDepth, d),
						_Utils_ap(foldDivs, divs));
				}),
			_Utils_Tuple3(width, depth, _List_Nil),
			children);
		var maxWidth = _v0.a;
		var maxDepth = _v0.b;
		var childDivs = _v0.c;
		return _Utils_Tuple3(
			maxWidth,
			maxDepth,
			A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('block'),
							A2(
							$elm$html$Html$Attributes$style,
							'grid-column',
							$elm$core$String$fromInt(width) + ('/' + $elm$core$String$fromInt(maxWidth))),
							A2(
							$elm$html$Html$Attributes$style,
							'grid-row',
							$elm$core$String$fromInt(-depth) + ('/' + $elm$core$String$fromInt((-depth) - 1))),
							$author$project$HtmlEvent$onClick(
							A2($author$project$Display$Select, eq, state.o))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							function () {
								switch (node.$) {
									case 0:
										var n = node.a;
										return $elm$core$String$fromFloat(n.ar);
									case 1:
										var n = node.a;
										return n.m;
									case 2:
										var n = node.a;
										return n.m;
									case 3:
										var n = node.a;
										return n.m;
									default:
										var n = node.a;
										return n.m;
								}
							}())
						])),
				childDivs));
	});
var $author$project$Display$stackedView_ = F3(
	function (eq, highlight, node) {
		var _v0 = A5($author$project$Display$stackRecursive, eq, highlight, 1, 1, node);
		var width = _v0.a;
		var depth = _v0.b;
		var divs = _v0.c;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('blocks'),
					A2(
					$elm$html$Html$Attributes$style,
					'grid-template-columns',
					'repeat(' + ($elm$core$String$fromInt(width - 1) + ', 1fr)')),
					A2(
					$elm$html$Html$Attributes$style,
					'grid-template-rows',
					'repeat(' + ($elm$core$String$fromInt(depth) + ', 1fr)'))
				]),
			divs);
	});
var $author$project$Display$view = F3(
	function (converter, attr, model) {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(attr, _List_Nil),
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (eqNum, _v0, result) {
						var root = _v0.b;
						var highlight = A2(
							$elm$core$Maybe$andThen,
							function (_v1) {
								var eq = _v1.a;
								var num = _v1.b;
								return _Utils_eq(eq, eqNum) ? $elm$core$Maybe$Just(num) : $elm$core$Maybe$Nothing;
							},
							model.x);
						return A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$map,
								converter,
								A2(
									$elm$html$Html$div,
									_List_Nil,
									_List_fromArray(
										[
											A3(
											$author$project$Display$collapsedView_,
											eqNum,
											highlight,
											$author$project$Math$symbolicate(root)),
											A3($author$project$Display$stackedView_, eqNum, highlight, root)
										]))),
							result);
					}),
				_List_Nil,
				model.F));
	});
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $author$project$Menu$Click = $elm$core$Basics$identity;
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Menu$partToHtml_ = F3(
	function (converter, model, part) {
		if (part.$ === 1) {
			var children = part.a;
			return A2($elm$html$Html$li, _List_Nil, children);
		} else {
			var name = part.a;
			var display = part.b;
			var children = part.c;
			var shown = display && A2($elm$core$Set$member, name, model.B);
			return A2(
				$elm$html$Html$li,
				display ? _List_fromArray(
					[
						$elm$html$Html$Attributes$class('menuSection'),
						$elm$html$Html$Attributes$class('shown')
					]) : _List_fromArray(
					[
						$elm$html$Html$Attributes$class('menuSection')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h1,
						_Utils_ap(
							shown ? _List_fromArray(
								[
									$elm$html$Html$Attributes$class('shown')
								]) : _List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('menuTitle'),
									$author$project$HtmlEvent$onClick(
									converter(name)),
									$elm$html$Html$Attributes$class('clickable')
								])),
						_List_fromArray(
							[
								$elm$html$Html$text(name)
							])),
						A2(
						$elm$html$Html$ul,
						shown ? _List_fromArray(
							[
								$elm$html$Html$Attributes$class('subMenu'),
								$elm$html$Html$Attributes$class('shown')
							]) : _List_fromArray(
							[
								$elm$html$Html$Attributes$class('subMenu')
							]),
						A2(
							$elm$core$List$map,
							A2($author$project$Menu$partToHtml_, converter, model),
							children))
					]));
		}
	});
var $author$project$Menu$view = F3(
	function (converter, model, children) {
		return A2(
			$elm$html$Html$nav,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('menu')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$ul,
					_List_Nil,
					A2(
						$elm$core$List$map,
						A2($author$project$Menu$partToHtml_, converter, model),
						children))
				]));
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $author$project$Notification$notificationAttr_ = F3(
	function (converter, id, deleting) {
		return deleting ? _List_fromArray(
			[
				$elm$html$Html$Attributes$class('notificationMessage'),
				$elm$html$Html$Attributes$class('deleting')
			]) : _List_fromArray(
			[
				$elm$html$Html$Attributes$class('notificationMessage'),
				$author$project$HtmlEvent$onClick(
				converter(
					$author$project$Notification$ClearEvent(id)))
			]);
	});
var $author$project$Notification$notificationDiv_ = F3(
	function (converter, id, _v0) {
		var deleting = _v0.a;
		var message = _v0.b;
		return _Utils_Tuple2(
			'notification-' + $elm$core$String$fromInt(id),
			A2(
				$elm$html$Html$div,
				A3($author$project$Notification$notificationAttr_, converter, id, deleting),
				_List_fromArray(
					[
						$author$project$Icon$cancel(
						_List_fromArray(
							[
								$author$project$Icon$class('clickable'),
								$author$project$Icon$class('cancelable')
							])),
						A2(
						$elm$html$Html$pre,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(message)
							]))
					])));
	});
var $author$project$Notification$view = F3(
	function (converter, attrs, model) {
		return A3(
			$elm$html$Html$Keyed$node,
			'div',
			_Utils_ap(attrs, _List_Nil),
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (id, val, result) {
						return A2(
							$elm$core$List$cons,
							A3($author$project$Notification$notificationDiv_, converter, id, val),
							result);
					}),
				_List_Nil,
				model.w));
	});
var $author$project$Tutorial$view = F3(
	function (converter, attrs, model) {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(attrs, _List_Nil),
			_List_Nil);
	});
var $author$project$Main$view = function (model) {
	return {
		bf: _Utils_ap(
			_List_fromArray(
				[
					A3(
					$author$project$Display$view,
					$author$project$Main$DisplayEvent,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('display')
						]),
					model.u),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('inputPane')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id('leftPane')
								]),
							_Utils_ap(
								_List_fromArray(
									[
										$author$project$Main$inputDiv(model)
									]),
								model.A ? _List_fromArray(
									[
										A2(
										$elm$html$Html$pre,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$id('helpText')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text($author$project$Math$notation)
											]))
									]) : _List_Nil)),
							A2(
							$elm$html$Html$div,
							A2(
								$elm$core$List$cons,
								$elm$html$Html$Attributes$id('rightPane'),
								model.W ? _List_Nil : _List_fromArray(
									[
										$elm$html$Html$Attributes$class('closed')
									])),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('menuToggle')
										]),
									_List_fromArray(
										[
											$author$project$Icon$menu(
											_List_fromArray(
												[
													$author$project$HtmlEvent$onClick($author$project$Main$ToggleMenu),
													$author$project$Icon$class('clickable'),
													$author$project$Icon$class('helpable')
												]))
										])),
									A3(
									$author$project$Menu$view,
									$author$project$Main$MenuEvent,
									model.U,
									_List_fromArray(
										[
											A3(
											$author$project$Menu$Section,
											'Settings',
											true,
											_List_fromArray(
												[
													$author$project$Menu$Content(
													_List_fromArray(
														[
															A2(
															$elm$html$Html$a,
															_List_Nil,
															_List_fromArray(
																[
																	$elm$html$Html$text('Load from Save File')
																]))
														])),
													$author$project$Menu$Content(
													_List_fromArray(
														[
															A2(
															$elm$html$Html$a,
															_List_Nil,
															_List_fromArray(
																[
																	$elm$html$Html$text('Save to Save File')
																]))
														])),
													$author$project$Menu$Content(
													_List_fromArray(
														[
															A2(
															$elm$html$Html$a,
															_List_fromArray(
																[
																	$author$project$HtmlEvent$onClick(
																	A2($author$project$Main$OpenDialog, 'Enter the url for the topic', $author$project$Main$DownloadTopic)),
																	$elm$html$Html$Attributes$class('clickable')
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text('Add Topic From URL')
																]))
														])),
													$author$project$Menu$Content(
													_List_fromArray(
														[
															A2(
															$elm$html$Html$a,
															_List_fromArray(
																[
																	$author$project$HtmlEvent$onClick(
																	$author$project$Main$FileSelect(0)),
																	$elm$html$Html$Attributes$class('clickable')
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text('Add Topic From File')
																]))
														])),
													$author$project$Menu$Content(
													_List_fromArray(
														[
															A2(
															$elm$html$Html$a,
															_List_fromArray(
																[
																	$author$project$HtmlEvent$onClick(
																	A2($author$project$Main$OpenDialog, 'Enter the topic to delete', $author$project$Main$DeleteTopic)),
																	$elm$html$Html$Attributes$class('clickable')
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text('Delete a Topic')
																]))
														]))
												])),
											A2($author$project$Tutorial$menu, $author$project$Main$TutorialEvent, model.P),
											A2($author$project$Rules$menuConstants, $author$project$Main$RulesEvent, model.j),
											A3(
											$author$project$Rules$menuFunctions,
											$author$project$Main$RulesEvent,
											model.j,
											$author$project$Main$inCreateMode_(model)),
											A3(
											$author$project$Rules$menuRules,
											$author$project$Main$RulesEvent,
											model.j,
											$author$project$Display$selectedNode(model.u))
										]))
								]))
						])),
					A3($author$project$Tutorial$view, $author$project$Main$TutorialEvent, _List_Nil, model.P),
					A3(
					$author$project$Notification$view,
					$author$project$Main$NotificationEvent,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('notification')
						]),
					model.H)
				]),
			$author$project$Main$dialogPane_(model.E)),
		bE: 'Maths'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{bp: $author$project$Main$init, bs: $author$project$Main$EventUrlChange, bt: $author$project$Main$EventUrlRequest, bC: $author$project$Main$subscriptions, bF: $author$project$Main$update, bH: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));