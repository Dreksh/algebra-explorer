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
	if (region.bh.aH === region.bs.aH)
	{
		return 'on line ' + region.bh.aH;
	}
	return 'on lines ' + region.bh.aH + ' through ' + region.bs.aH;
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
		impl.ck,
		impl.cA,
		impl.cy,
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
		aU: func(record.aU),
		aZ: record.aZ,
		aV: record.aV
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
		var message = !tag ? value : tag < 3 ? value.a : value.aU;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aZ;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aV) && event.preventDefault(),
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
		impl.ck,
		impl.cA,
		impl.cy,
		function(sendToApp, initialModel) {
			var view = impl.cB;
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
		impl.ck,
		impl.cA,
		impl.cy,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.bg && impl.bg(sendToApp)
			var view = impl.cB;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.ca);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.a$) && (_VirtualDom_doc.title = title = doc.a$);
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
	var onUrlChange = impl.cn;
	var onUrlRequest = impl.co;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		bg: function(sendToApp)
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
							&& curr.bQ === next.bQ
							&& curr.bA === next.bA
							&& curr.bL.a === next.bL.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		ck: function(flags)
		{
			return A3(impl.ck, flags, _Browser_getUrl(), key);
		},
		cB: impl.cB,
		cA: impl.cA,
		cy: impl.cy
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
		? { P: 'hidden', cb: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { P: 'mozHidden', cb: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { P: 'msHidden', cb: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { P: 'webkitHidden', cb: 'webkitvisibilitychange' }
		: { P: 'hidden', cb: 'visibilitychange' };
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
		bX: _Browser_getScene(),
		b4: {
			b5: _Browser_window.pageXOffset,
			b6: _Browser_window.pageYOffset,
			J: _Browser_doc.documentElement.clientWidth,
			F: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		J: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		F: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			bX: {
				J: node.scrollWidth,
				F: node.scrollHeight
			},
			b4: {
				b5: node.scrollLeft,
				b6: node.scrollTop,
				J: node.clientWidth,
				F: node.clientHeight
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
			bX: _Browser_getScene(),
			b4: {
				b5: x,
				b6: y,
				J: _Browser_doc.documentElement.clientWidth,
				F: _Browser_doc.documentElement.clientHeight
			},
			br: {
				b5: x + rect.left,
				b6: y + rect.top,
				J: rect.width,
				F: rect.height
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



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.bu.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.bu.b, xhr)); });
		$elm$core$Maybe$isJust(request.m) && _Http_track(router, xhr, request.m.a);

		try {
			xhr.open(request.cm, request.b3, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.b3));
		}

		_Http_configureRequest(xhr, request);

		request.ca.a && xhr.setRequestHeader('Content-Type', request.ca.a);
		xhr.send(request.ca.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.by; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.cz.a || 0;
	xhr.responseType = request.bu.d;
	xhr.withCredentials = request.b8;
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
		b3: xhr.responseURL,
		cv: xhr.status,
		cw: xhr.statusText,
		by: _Http_parseHeaders(xhr.getAllResponseHeaders())
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
			cu: event.loaded,
			aX: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			cr: event.loaded,
			aX: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
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
		if (!builder.r) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.s),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.s);
		} else {
			var treeLen = builder.r * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.u) : builder.u;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.r);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.s) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.s);
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
					{u: nodeList, r: (len / $elm$core$Array$branchFactor) | 0, s: tail});
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
		return {bx: fragment, bA: host, bJ: path, bL: port_, bQ: protocol, bR: query};
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
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$UI$Notification$ClearEvent = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$UI$Animation$delayEvent = F2(
	function (timeout, e) {
		return A2(
			$elm$core$Task$perform,
			function (_v0) {
				return e;
			},
			$elm$core$Process$sleep(timeout));
	});
var $author$project$UI$Notification$DeleteEvent = function (a) {
	return {$: 1, a: a};
};
var $author$project$UI$Notification$delayedDelete_ = F2(
	function (id, _v0) {
		return A2(
			$author$project$UI$Animation$delayEvent,
			750,
			$author$project$UI$Notification$DeleteEvent(id));
	});
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
var $author$project$UI$Animation$newDeletable = F2(
	function (trigger, element) {
		return {ce: false, br: element, a0: trigger};
	});
var $author$project$UI$Notification$displayError = F2(
	function (str, _v0) {
		var model = _v0.a;
		var cmd = _v0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					o: model.o + 1,
					M: A3(
						$elm$core$Dict$insert,
						model.o,
						A2(
							$author$project$UI$Animation$newDeletable,
							$author$project$UI$Notification$delayedDelete_(model.o),
							str),
						model.M)
				}),
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						cmd,
						A2(
						$author$project$UI$Animation$delayEvent,
						15000,
						$author$project$UI$Notification$ClearEvent(model.o))
					])));
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$evaluateString = _Platform_outgoingPort(
	'evaluateString',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$int($.ci)),
					_Utils_Tuple2(
					'str',
					$elm$json$Json$Encode$string($.cx))
				]));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Main$NoOp = {$: 8};
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
var $author$project$Algo$History$init = function (c) {
	return {
		R: $elm$core$Dict$empty,
		ar: {af: _List_Nil, at: c, aJ: 0},
		ad: _List_Nil,
		I: _List_fromArray(
			[0])
	};
};
var $author$project$Components$Display$init = function (eqs) {
	return {
		aR: $elm$core$Maybe$Nothing,
		f: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (index, eq) {
						return _Utils_Tuple2(
							index,
							$author$project$Algo$History$init(eq));
					}),
				eqs)),
		P: $elm$core$Set$empty,
		an: $elm$core$List$length(eqs),
		n: $elm$core$Maybe$Nothing
	};
};
var $author$project$Components$Evaluate$Model = F3(
	function (nextCallID, ongoing, sender) {
		return {al: nextCallID, ab: ongoing, be: sender};
	});
var $author$project$Components$Evaluate$init = A2($author$project$Components$Evaluate$Model, 0, $elm$core$Dict$empty);
var $author$project$Components$Rules$InfixOp = function (a) {
	return {$: 0, a: a};
};
var $author$project$Components$Rules$PrefixOp = function (a) {
	return {$: 1, a: a};
};
var $author$project$Components$Rules$init = {
	C: $elm$core$Dict$empty,
	y: $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'+',
				_Utils_Tuple2(
					{
						ak: $author$project$Components$Rules$InfixOp('+'),
						k: {e: 2, j: true, g: true}
					},
					1)),
				_Utils_Tuple2(
				'*',
				_Utils_Tuple2(
					{
						ak: $author$project$Components$Rules$InfixOp('*'),
						k: {e: 2, j: true, g: true}
					},
					1)),
				_Utils_Tuple2(
				'-',
				_Utils_Tuple2(
					{
						ak: $author$project$Components$Rules$PrefixOp('-'),
						k: {e: 1, j: false, g: false}
					},
					1)),
				_Utils_Tuple2(
				'/',
				_Utils_Tuple2(
					{
						ak: $author$project$Components$Rules$PrefixOp('1/'),
						k: {e: 1, j: false, g: false}
					},
					1))
			])),
	v: $elm$core$Dict$empty
};
var $author$project$Components$Tutorial$init = {bE: 0, bY: 0};
var $author$project$UI$Draggable$init = F3(
	function (id, _v0, _v1) {
		var left = _v0.a;
		var top = _v0.b;
		var width = _v1.a;
		var height = _v1.b;
		return {
			q: {B: top + height, w: left, A: left + width, E: top},
			ah: $elm$core$Dict$empty,
			ci: id
		};
	});
var $author$project$UI$Menu$init = function (shown) {
	return {V: shown};
};
var $author$project$UI$Notification$init = {o: 0, M: $elm$core$Dict$empty};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Main$ProcessSource = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
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
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
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
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
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
					$elm$http$Http$BadStatus(metadata.cv));
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
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {bT: reqs, b0: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
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
							var _v4 = req.m;
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
			A3($elm$http$Http$updateReqs, router, cmds, state.bT));
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
					state.b0)));
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
					b8: r.b8,
					ca: r.ca,
					bu: A2(_Http_mapExpect, func, r.bu),
					by: r.by,
					cm: r.cm,
					cz: r.cz,
					m: r.m,
					b3: r.b3
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
			{b8: false, ca: r.ca, bu: r.bu, by: r.by, cm: r.cm, cz: r.cz, m: r.m, b3: r.b3}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{ca: $elm$http$Http$emptyBody, bu: r.bu, by: _List_Nil, cm: 'GET', cz: $elm$core$Maybe$Nothing, m: $elm$core$Maybe$Nothing, b3: r.b3});
};
var $author$project$Main$Source = function (topics) {
	return {v: topics};
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$sourceDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Main$Source,
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$field,
				'topics',
				$elm$json$Json$Decode$dict($elm$json$Json$Decode$string)),
				$elm$json$Json$Decode$succeed($elm$core$Dict$empty)
			])));
var $author$project$Main$loadSources = function (sources) {
	return $elm$core$Platform$Cmd$batch(
		A2(
			$elm$core$List$map,
			function (url) {
				return $elm$http$Http$get(
					{
						bu: A2(
							$elm$http$Http$expectJson,
							$author$project$Main$ProcessSource(url),
							$author$project$Main$sourceDecoder),
						b3: url
					});
			},
			A2($elm$core$List$cons, 'source.json', sources)));
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
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
var $author$project$Components$Display$createState = function (num) {
	return {ap: num};
};
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
var $author$project$Algo$Math$deadEndToString_ = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$map(
		function (deadEnd) {
			return function (str) {
				return str + (', at position: ' + $elm$core$String$fromInt(deadEnd.cc));
			}(
				function () {
					var _v0 = deadEnd.cq;
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
var $author$project$Algo$Math$createErrorMessage_ = F2(
	function (str, err) {
		return 'Error parsing \"' + (str + ('\":\n' + $author$project$Algo$Math$deadEndToString_(err)));
	});
var $elm$parser$Parser$ExpectingEnd = {$: 10};
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {cc: col, cd: contextStack, cq: problem, bW: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.bW, s.cc, x, s.h));
	});
var $elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			$elm$core$String$length(s.c),
			s.d) ? A3($elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $author$project$Algo$Math$DeclarativeNode = function (a) {
	return {$: 5, a: a};
};
var $elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var $author$project$Algo$Math$BinaryNode = function (a) {
	return {$: 3, a: a};
};
var $author$project$Algo$Math$GenericNode = function (a) {
	return {$: 4, a: a};
};
var $author$project$Algo$Math$UnaryNode = function (a) {
	return {$: 2, a: a};
};
var $author$project$Algo$Math$VariableNode = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
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
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.c);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.d, offset) < 0,
					0,
					{cc: col, h: s0.h, l: s0.l, d: offset, bW: row, c: s0.c});
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
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.d, s.bW, s.cc, s);
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
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.d, s.bW, s.cc, s.c);
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
			{cc: newCol, h: s.h, l: s.l, d: newOffset, bW: newRow, c: s.c});
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
var $author$project$Algo$Math$validVarStart_ = function (_char) {
	return !A2(
		$elm$core$String$contains,
		$elm$core$String$fromChar(_char),
		' ~+-=*/\\.()[],;%!:;<>0123456789^&|$↔→');
};
var $elm$parser$Parser$ExpectingVariable = {$: 7};
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
				return {cc: col, h: context, l: indent, d: offset, bW: row, c: src};
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
		var firstOffset = A3($elm$parser$Parser$Advanced$isSubChar, i.bh, s.d, s.c);
		if (_Utils_eq(firstOffset, -1)) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.bv));
		} else {
			var s1 = _Utils_eq(firstOffset, -2) ? A7($elm$parser$Parser$Advanced$varHelp, i.a5, s.d + 1, s.bW + 1, 1, s.c, s.l, s.h) : A7($elm$parser$Parser$Advanced$varHelp, i.a5, firstOffset, s.bW, s.cc + 1, s.c, s.l, s.h);
			var name = A3($elm$core$String$slice, s.d, s1.d, s.c);
			return A2($elm$core$Set$member, name, i.bc) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.bv)) : A3($elm$parser$Parser$Advanced$Good, true, name, s1);
		}
	};
};
var $elm$parser$Parser$variable = function (i) {
	return $elm$parser$Parser$Advanced$variable(
		{bv: $elm$parser$Parser$ExpectingVariable, a5: i.a5, bc: i.bc, bh: i.bh});
};
var $author$project$Algo$Math$tokenLongName_ = $elm$parser$Parser$variable(
	{
		a5: function (c) {
			return $author$project$Algo$Math$validVarStart_(c) || $elm$core$Char$isAlphaNum(c);
		},
		bc: $elm$core$Set$empty,
		bh: $author$project$Algo$Math$validVarStart_
	});
var $author$project$Algo$Math$RealNode = function (a) {
	return {$: 0, a: a};
};
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
var $author$project$Algo$Math$tokenDigit_ = $elm$parser$Parser$variable(
	{a5: $elm$core$Char$isDigit, bc: $elm$core$Set$empty, bh: $elm$core$Char$isDigit});
var $author$project$Algo$Math$tokenNumber_ = A2(
	$elm$parser$Parser$andThen,
	function (str) {
		var _v0 = $elm$core$String$toFloat(str);
		if (_v0.$ === 1) {
			return $elm$parser$Parser$problem(str + ' is not a valid number');
		} else {
			var f = _v0.a;
			return $elm$parser$Parser$succeed(
				$author$project$Algo$Math$RealNode(
					{b: 0, x: f}));
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
			$author$project$Algo$Math$tokenDigit_),
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
					$author$project$Algo$Math$tokenDigit_),
					$elm$parser$Parser$succeed('')
				]))));
var $author$project$Algo$Math$tokenShortName_ = $elm$parser$Parser$variable(
	{
		a5: function (_v0) {
			return false;
		},
		bc: $elm$core$Set$empty,
		bh: $author$project$Algo$Math$validVarStart_
	});
function $author$project$Algo$Math$cyclic$negatable_() {
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
								return $author$project$Algo$Math$UnaryNode(
									{N: x, a: '-', b: 0});
							}),
						$elm$parser$Parser$symbol('-')),
					$elm$parser$Parser$spaces),
				$elm$parser$Parser$lazy(
					function (_v7) {
						return $author$project$Algo$Math$cyclic$negatable_();
					})),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$author$project$Algo$Math$cyclic$term_())
			]));
}
function $author$project$Algo$Math$cyclic$term_() {
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
						$author$project$Algo$Math$cyclic$expression_(),
						$elm$parser$Parser$spaces),
					$elm$parser$Parser$symbol(')'))),
				$author$project$Algo$Math$tokenNumber_,
				A2(
				$elm$parser$Parser$map,
				function (_v5) {
					var name = _v5.a;
					var props = _v5.b;
					if (props.$ === 1) {
						return $author$project$Algo$Math$VariableNode(
							{a: name, b: 0});
					} else {
						var children = props.a;
						return $author$project$Algo$Math$GenericNode(
							{af: children, a: name, b: 0});
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
						$author$project$Algo$Math$tokenLongName_),
					$author$project$Algo$Math$cyclic$varOrFunc_())),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					function (name) {
						return $author$project$Algo$Math$VariableNode(
							{a: name, b: 0});
					}),
				$author$project$Algo$Math$tokenShortName_)
			]));
}
function $author$project$Algo$Math$cyclic$varOrFunc_() {
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
					$author$project$Algo$Math$cyclic$argsList_(),
					$elm$parser$Parser$symbol(')'))),
				$elm$parser$Parser$succeed($elm$core$Maybe$Nothing)
			]));
}
function $author$project$Algo$Math$cyclic$argsList_() {
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
					$author$project$Algo$Math$cyclic$expression_(),
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
							$author$project$Algo$Math$cyclic$expression_(),
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
function $author$project$Algo$Math$cyclic$expression_() {
	return A2(
		$elm$parser$Parser$map,
		function (children) {
			if (children.b && (!children.b.b)) {
				var x = children.a;
				return x;
			} else {
				return $author$project$Algo$Math$BinaryNode(
					{j: true, af: children, g: true, a: '+', b: 0});
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
						$author$project$Algo$Math$cyclic$multiple_(),
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
								$author$project$Algo$Math$cyclic$multiple_(),
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
												$author$project$Algo$Math$UnaryNode(
													{N: elem, a: '-', b: 0}),
												list));
									}),
								$elm$parser$Parser$symbol('-')),
							A2(
								$elm$parser$Parser$ignorer,
								$author$project$Algo$Math$cyclic$multiple_(),
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
function $author$project$Algo$Math$cyclic$multiple_() {
	return A2(
		$elm$parser$Parser$map,
		function (children) {
			if (children.b && (!children.b.b)) {
				var x = children.a;
				return x;
			} else {
				return $author$project$Algo$Math$BinaryNode(
					{j: true, af: children, g: true, a: '*', b: 0});
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
						$author$project$Algo$Math$cyclic$negatable_(),
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
								$author$project$Algo$Math$cyclic$negatable_(),
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
												$author$project$Algo$Math$UnaryNode(
													{N: elem, a: '/', b: 0}),
												list));
									}),
								$elm$parser$Parser$symbol('/')),
							A2(
								$elm$parser$Parser$ignorer,
								$author$project$Algo$Math$cyclic$negatable_(),
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
								$author$project$Algo$Math$cyclic$term_(),
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
var $author$project$Algo$Math$negatable_ = $author$project$Algo$Math$cyclic$negatable_();
$author$project$Algo$Math$cyclic$negatable_ = function () {
	return $author$project$Algo$Math$negatable_;
};
var $author$project$Algo$Math$term_ = $author$project$Algo$Math$cyclic$term_();
$author$project$Algo$Math$cyclic$term_ = function () {
	return $author$project$Algo$Math$term_;
};
var $author$project$Algo$Math$varOrFunc_ = $author$project$Algo$Math$cyclic$varOrFunc_();
$author$project$Algo$Math$cyclic$varOrFunc_ = function () {
	return $author$project$Algo$Math$varOrFunc_;
};
var $author$project$Algo$Math$argsList_ = $author$project$Algo$Math$cyclic$argsList_();
$author$project$Algo$Math$cyclic$argsList_ = function () {
	return $author$project$Algo$Math$argsList_;
};
var $author$project$Algo$Math$expression_ = $author$project$Algo$Math$cyclic$expression_();
$author$project$Algo$Math$cyclic$expression_ = function () {
	return $author$project$Algo$Math$expression_;
};
var $author$project$Algo$Math$multiple_ = $author$project$Algo$Math$cyclic$multiple_();
$author$project$Algo$Math$cyclic$multiple_ = function () {
	return $author$project$Algo$Math$multiple_;
};
var $author$project$Algo$Math$equation_ = A2(
	$elm$parser$Parser$map,
	function (children) {
		if (children.b && (!children.b.b)) {
			var x = children.a;
			return x;
		} else {
			return $author$project$Algo$Math$DeclarativeNode(
				{af: children, a: '=', b: 0});
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
				A2($elm$parser$Parser$ignorer, $author$project$Algo$Math$expression_, $elm$parser$Parser$spaces)) : $elm$parser$Parser$oneOf(
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
						A2($elm$parser$Parser$ignorer, $author$project$Algo$Math$expression_, $elm$parser$Parser$spaces)),
						A2(
						$elm$parser$Parser$map,
						function (_v0) {
							return $elm$parser$Parser$Done(
								$elm$core$List$reverse(list));
						},
						$elm$parser$Parser$succeed(0))
					]));
		}));
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {cc: col, cq: problem, bW: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.bW, p.cc, p.cq);
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
			{cc: 1, h: _List_Nil, l: 1, d: 0, bW: 1, c: src});
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
var $author$project$Algo$Math$parse = function (input) {
	return A2(
		$elm$core$Result$mapError,
		$author$project$Algo$Math$createErrorMessage_(input),
		A2(
			$elm$parser$Parser$run,
			A2($elm$parser$Parser$ignorer, $author$project$Algo$Math$equation_, $elm$parser$Parser$end),
			input));
};
var $author$project$Algo$Matcher$State_ = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Algo$Matcher$addNode_ = F3(
	function (previous, parent, tracker) {
		return _Utils_Tuple2(
			A2(
				$author$project$Algo$Matcher$State_,
				tracker.o,
				function () {
					if (previous.$ === 1) {
						return tracker.aI(tracker.o);
					} else {
						var prev = previous.a;
						return A2(tracker.aF, prev, tracker.o);
					}
				}()),
			_Utils_update(
				tracker,
				{
					o: tracker.o + 1,
					aJ: A3($elm$core$Dict$insert, tracker.o, parent, tracker.aJ)
				}));
	});
var $author$project$Algo$Matcher$getID = function (s) {
	var rootID = s.a;
	return rootID;
};
var $author$project$Algo$Math$map_ = F4(
	function (transition, parent, glob, root) {
		var _v0 = A3(transition, parent, root, glob);
		var finS = _v0.a;
		var nextG = _v0.b;
		var processChildren = function (nextGlob) {
			return A2(
				$elm$core$List$foldl,
				F2(
					function (child, _v6) {
						var list = _v6.a;
						var g = _v6.b;
						return function (_v7) {
							var c = _v7.a;
							var finG = _v7.b;
							return _Utils_Tuple2(
								A2($elm$core$List$cons, c, list),
								finG);
						}(
							A4(
								$author$project$Algo$Math$map_,
								transition,
								$elm$core$Maybe$Just(finS),
								g,
								child));
					}),
				_Utils_Tuple2(_List_Nil, nextGlob));
		};
		switch (root.$) {
			case 0:
				var n = root.a;
				return _Utils_Tuple2(
					$author$project$Algo$Math$RealNode(
						{b: finS, x: n.x}),
					nextG);
			case 1:
				var n = root.a;
				return _Utils_Tuple2(
					$author$project$Algo$Math$VariableNode(
						{a: n.a, b: finS}),
					nextG);
			case 2:
				var n = root.a;
				return function (_v2) {
					var finChild = _v2.a;
					var finGlob = _v2.b;
					return _Utils_Tuple2(
						$author$project$Algo$Math$UnaryNode(
							{N: finChild, a: n.a, b: finS}),
						finGlob);
				}(
					A4(
						$author$project$Algo$Math$map_,
						transition,
						$elm$core$Maybe$Just(finS),
						nextG,
						n.N));
			case 3:
				var n = root.a;
				return function (_v3) {
					var finChildren = _v3.a;
					var finGlob = _v3.b;
					return _Utils_Tuple2(
						$author$project$Algo$Math$BinaryNode(
							{
								j: n.j,
								af: $elm$core$List$reverse(finChildren),
								g: n.g,
								a: n.a,
								b: finS
							}),
						finGlob);
				}(
					A2(processChildren, nextG, n.af));
			case 4:
				var n = root.a;
				return function (_v4) {
					var finChildren = _v4.a;
					var finGlob = _v4.b;
					return _Utils_Tuple2(
						$author$project$Algo$Math$GenericNode(
							{
								af: $elm$core$List$reverse(finChildren),
								a: n.a,
								b: finS
							}),
						finGlob);
				}(
					A2(processChildren, nextG, n.af));
			default:
				var n = root.a;
				return function (_v5) {
					var finChildren = _v5.a;
					var finGlob = _v5.b;
					return _Utils_Tuple2(
						$author$project$Algo$Math$DeclarativeNode(
							{
								af: $elm$core$List$reverse(finChildren),
								a: n.a,
								b: finS
							}),
						finGlob);
				}(
					A2(processChildren, nextG, n.af));
		}
	});
var $author$project$Algo$Math$map = F3(
	function (transition, glob, root) {
		return A4($author$project$Algo$Math$map_, transition, $elm$core$Maybe$Nothing, glob, root);
	});
var $author$project$Algo$Matcher$processID_ = $author$project$Algo$Math$map(
	F3(
		function (p, _v0, t) {
			if (p.$ === 1) {
				return _Utils_Tuple2(
					A2(
						$author$project$Algo$Matcher$State_,
						t.o,
						t.aI(t.o)),
					_Utils_update(
						t,
						{o: t.o + 1}));
			} else {
				var pState = p.a;
				return A3(
					$author$project$Algo$Matcher$addNode_,
					$elm$core$Maybe$Nothing,
					$author$project$Algo$Matcher$getID(pState),
					t);
			}
		}));
var $author$project$Algo$Matcher$parseEquation = F2(
	function (newState, copyState) {
		return A2(
			$elm$core$Basics$composeR,
			$author$project$Algo$Matcher$processID_(
				{aF: copyState, aI: newState, o: 0, aJ: $elm$core$Dict$empty}),
			function (_v0) {
				var newRoot = _v0.a;
				var tracker = _v0.b;
				return {
					ar: newRoot,
					m: _Utils_update(
						tracker,
						{
							aJ: A2($elm$core$Dict$remove, 0, tracker.aJ)
						})
				};
			});
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Helper$resultList = F2(
	function (process, start) {
		return A2(
			$elm$core$List$foldl,
			F2(
				function (elem, res) {
					return A2(
						$elm$core$Result$andThen,
						process(elem),
						res);
				}),
			$elm$core$Result$Ok(start));
	});
var $author$project$Components$Rules$replaceGlobalVar = F2(
	function (model, root) {
		var processChildren = A2(
			$elm$core$Basics$composeR,
			A2(
				$author$project$Helper$resultList,
				F2(
					function (child, list) {
						return A2(
							$elm$core$Result$map,
							function (c) {
								return A2($elm$core$List$cons, c, list);
							},
							A2($author$project$Components$Rules$replaceGlobalVar, model, child));
					}),
				_List_Nil),
			$elm$core$Result$map($elm$core$List$reverse));
		switch (root.$) {
			case 2:
				var s = root.a;
				return A2(
					$elm$core$Result$map,
					function (child) {
						return $author$project$Algo$Math$UnaryNode(
							_Utils_update(
								s,
								{N: child}));
					},
					A2($author$project$Components$Rules$replaceGlobalVar, model, s.N));
			case 3:
				var s = root.a;
				return A2(
					$elm$core$Result$map,
					function (children) {
						return $author$project$Algo$Math$BinaryNode(
							_Utils_update(
								s,
								{af: children}));
					},
					processChildren(s.af));
			case 5:
				var s = root.a;
				return A2(
					$elm$core$Result$map,
					function (children) {
						return $author$project$Algo$Math$DeclarativeNode(
							_Utils_update(
								s,
								{af: children}));
					},
					processChildren(s.af));
			case 4:
				var s = root.a;
				return A2(
					$elm$core$Result$andThen,
					function (children) {
						var _v1 = A2($elm$core$Dict$get, s.a, model.y);
						if (_v1.$ === 1) {
							return $elm$core$Result$Ok(
								$author$project$Algo$Math$GenericNode(
									_Utils_update(
										s,
										{af: children})));
						} else {
							var _v2 = _v1.a;
							var f = _v2.a;
							if ((f.k.e === 2) && (f.k.j || f.k.g)) {
								return $elm$core$Result$Ok(
									$author$project$Algo$Math$BinaryNode(
										{j: f.k.j, af: children, g: f.k.g, a: s.a, b: s.b}));
							} else {
								if (!_Utils_eq(
									$elm$core$List$length(s.af),
									f.k.e)) {
									return $elm$core$Result$Err('Unexpected number of arguments in ' + s.a);
								} else {
									if (children.b && (!children.b.b)) {
										var child = children.a;
										return $elm$core$Result$Ok(
											$author$project$Algo$Math$UnaryNode(
												{N: child, a: s.a, b: s.b}));
									} else {
										return $elm$core$Result$Ok(
											$author$project$Algo$Math$GenericNode(
												_Utils_update(
													s,
													{af: children})));
									}
								}
							}
						}
					},
					processChildren(s.af));
			default:
				return $elm$core$Result$Ok(root);
		}
	});
var $author$project$Components$Display$updateState = F2(
	function (s, _v0) {
		return {
			ap: $author$project$Algo$Matcher$getID(s)
		};
	});
var $author$project$Main$parseEquations_ = F3(
	function (model, elem, _v0) {
		var result = _v0.a;
		var errs = _v0.b;
		var _v1 = A2(
			$elm$core$Result$map,
			A2($author$project$Algo$Matcher$parseEquation, $author$project$Components$Display$createState, $author$project$Components$Display$updateState),
			A2(
				$elm$core$Result$andThen,
				$author$project$Components$Rules$replaceGlobalVar(model),
				$author$project$Algo$Math$parse(elem)));
		if (!_v1.$) {
			var root = _v1.a;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, root, result),
				errs);
		} else {
			var err = _v1.a;
			return _Utils_Tuple2(
				result,
				A2($elm$core$List$cons, err, errs));
		}
	});
var $elm$core$String$indices = _String_indexes;
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $author$project$Components$Query$unmarshalModel_ = F2(
	function (line, model) {
		return function (_v1) {
			var field = _v1.a;
			var value = _v1.b;
			switch (field) {
				case 'eq':
					if (!value.$) {
						var str = value.a;
						return _Utils_update(
							model,
							{
								f: _Utils_ap(
									model.f,
									_List_fromArray(
										[str]))
							});
					} else {
						return model;
					}
				case 'source':
					if (!value.$) {
						var str = value.a;
						return _Utils_update(
							model,
							{
								aY: _Utils_ap(
									model.aY,
									_List_fromArray(
										[str]))
							});
					} else {
						return model;
					}
				default:
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
var $author$project$Components$Query$parseInit = F2(
	function (url, key) {
		return A3(
			$elm$core$List$foldl,
			$author$project$Components$Query$unmarshalModel_,
			{a1: url, f: _List_Nil, a6: key, aY: _List_Nil},
			A2(
				$elm$core$String$split,
				'&',
				A2($elm$core$Maybe$withDefault, '', url.bR)));
	});
var $elm$core$Result$toMaybe = function (result) {
	if (!result.$) {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$DeleteCreateMode = function (a) {
	return {$: 12, a: a};
};
var $author$project$Main$uiCancelCmd_ = F2(
	function (num, _v0) {
		return A2(
			$author$project$UI$Animation$delayEvent,
			500,
			$author$project$Main$DeleteCreateMode(num));
	});
var $author$project$Main$init = F3(
	function (flags, url, key) {
		var query = A2($author$project$Components$Query$parseInit, url, key);
		var _v0 = function (_v1) {
			var a = _v1.a;
			var b = _v1.b;
			return _Utils_Tuple2(
				$elm$core$List$reverse(a),
				$elm$core$List$reverse(b));
		}(
			A3(
				$elm$core$List$foldl,
				$author$project$Main$parseEquations_($author$project$Components$Rules$init),
				_Utils_Tuple2(_List_Nil, _List_Nil),
				query.f));
		var eqs = _v0.a;
		var errs = _v0.b;
		var newScreen = $elm$core$List$isEmpty(eqs);
		var _v2 = A3(
			$elm$core$List$foldl,
			$author$project$UI$Notification$displayError,
			_Utils_Tuple2($author$project$UI$Notification$init, $elm$core$Platform$Cmd$none),
			errs);
		var nModel = _v2.a;
		var nCmd = _v2.b;
		return _Utils_Tuple2(
			{
				t: $elm$core$Maybe$Nothing,
				bR: query,
				aX: A2(
					$elm$core$Maybe$withDefault,
					_Utils_Tuple2(0, 0),
					$elm$core$Result$toMaybe(
						A2(
							$elm$json$Json$Decode$decodeValue,
							A3(
								$elm$json$Json$Decode$map2,
								$elm$core$Tuple$pair,
								A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
								A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float)),
							flags))),
				H: {
					D: newScreen ? $elm$core$Maybe$Just(
						A2(
							$author$project$UI$Animation$newDeletable,
							$author$project$Main$uiCancelCmd_(0),
							0)) : $elm$core$Maybe$Nothing,
					i: $author$project$Components$Display$init(eqs),
					O: $author$project$Components$Evaluate$init($author$project$Main$evaluateString),
					av: A3(
						$author$project$UI$Draggable$init,
						'history',
						_Utils_Tuple2(60, 10),
						_Utils_Tuple2(30, 80)),
					ax: $author$project$UI$Menu$init(
						$elm$core$Set$fromList(
							_List_fromArray(
								['Settings', 'Equations', 'Tutorials']))),
					am: newScreen ? 1 : 0,
					aa: nModel,
					bd: $author$project$Components$Rules$init,
					U: false,
					az: false,
					Y: false,
					aC: $author$project$Components$Tutorial$init
				}
			},
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $author$project$Main$NotificationEvent, nCmd),
						newScreen ? $author$project$Main$focusTextBar_('textInput') : $elm$core$Platform$Cmd$none,
						$author$project$Main$loadSources(query.aY)
					])));
	});
var $author$project$Main$EvalComplete = function (a) {
	return {$: 29, a: a};
};
var $author$project$Main$PressedKey = function (a) {
	return {$: 9, a: a};
};
var $author$project$Main$WindowResize = F2(
	function (a, b) {
		return {$: 25, a: a, b: b};
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Main$evaluateResult = _Platform_incomingPort(
	'evaluateResult',
	A2(
		$elm$json$Json$Decode$andThen,
		function (value) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (id) {
					return $elm$json$Json$Decode$succeed(
						{ci: id, x: value});
				},
				A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int));
		},
		A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$float)));
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Main$onKeyDown = _Platform_incomingPort(
	'onKeyDown',
	A2(
		$elm$json$Json$Decode$andThen,
		function (shift) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (key) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (ctrl) {
							return $elm$json$Json$Decode$succeed(
								{bo: ctrl, a6: key, b_: shift});
						},
						A2($elm$json$Json$Decode$field, 'ctrl', $elm$json$Json$Decode$bool));
				},
				A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'shift', $elm$json$Json$Decode$bool)));
var $elm$browser$Browser$Events$Window = 1;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {bK: pids, b0: subs};
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
		return {cg: event, a6: key};
	});
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
			state.bK,
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
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.a6;
		var event = _v0.cg;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.b0);
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
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		1,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Main$onKeyDown($author$project$Main$PressedKey),
				$author$project$Main$evaluateResult($author$project$Main$EvalComplete),
				$elm$browser$Browser$Events$onResize($author$project$Main$WindowResize)
			]));
};
var $author$project$Main$DisplayEvent = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$EvalType_ = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Main$FileLoaded = F2(
	function (a, b) {
		return {$: 23, a: a, b: b};
	});
var $author$project$Main$FileSelected = F2(
	function (a, b) {
		return {$: 22, a: a, b: b};
	});
var $author$project$Main$NumSubType_ = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $author$project$Main$ProcessTopic = F2(
	function (a, b) {
		return {$: 19, a: a, b: b};
	});
var $author$project$Main$TutorialEvent = function (a) {
	return {$: 4, a: a};
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $author$project$Main$capture = _Platform_outgoingPort(
	'capture',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'eId',
					$elm$json$Json$Encode$string($.a2)),
					_Utils_Tuple2(
					'pId',
					$elm$core$Basics$identity($.ba)),
					_Utils_Tuple2(
					'set',
					$elm$json$Json$Encode$bool($.bf))
				]));
	});
var $author$project$Main$actionToCapture_ = function (action) {
	if (action.$ === 1) {
		return $elm$core$Platform$Cmd$none;
	} else {
		if (!action.a.$) {
			var _v1 = action.a;
			var eId = _v1.a;
			var pId = _v1.b;
			return $author$project$Main$capture(
				{a2: eId, ba: pId, bf: true});
		} else {
			var _v2 = action.a;
			var eId = _v2.a;
			var pId = _v2.b;
			return $author$project$Main$capture(
				{a2: eId, ba: pId, bf: false});
		}
	}
};
var $author$project$Components$Display$addEquation = F2(
	function (eq, model) {
		return _Utils_update(
			model,
			{
				f: A3(
					$elm$core$Dict$insert,
					model.an,
					$author$project$Algo$History$init(eq),
					model.f),
				an: model.an + 1
			});
	});
var $author$project$Algo$Matcher$ExternalValue_ = function (a) {
	return {$: 0, a: a};
};
var $author$project$Algo$Matcher$addMatch = F2(
	function (key, replacement) {
		return A2(
			$elm$core$Dict$insert,
			key,
			$author$project$Algo$Matcher$ExternalValue_(replacement));
	});
var $author$project$Components$Rules$Installed_ = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Components$Rules$NotInstalled_ = function (a) {
	return {$: 0, a: a};
};
var $author$project$Components$Rules$addSources = F2(
	function (map, model) {
		return _Utils_update(
			model,
			{
				v: A3(
					$elm$core$Dict$foldl,
					F3(
						function (name, url, dict) {
							var _v0 = A2($elm$core$Dict$get, name, dict);
							if (_v0.$ === 1) {
								return A3(
									$elm$core$Dict$insert,
									name,
									$author$project$Components$Rules$NotInstalled_(url),
									dict);
							} else {
								var existing = _v0.a;
								if ((existing.$ === 1) && (existing.a.$ === 1)) {
									var _v2 = existing.a;
									var obj = existing.b;
									return A3(
										$elm$core$Dict$insert,
										name,
										A2(
											$author$project$Components$Rules$Installed_,
											$elm$core$Maybe$Just(url),
											obj),
										dict);
								} else {
									return dict;
								}
							}
						}),
					model.v,
					map)
			});
	});
var $author$project$Components$Rules$deleteTopic = F2(
	function (name, model) {
		var _v0 = A2($elm$core$Dict$get, name, model.v);
		if ((!_v0.$) && (_v0.a.$ === 1)) {
			var _v1 = _v0.a;
			var url = _v1.a;
			var topic = _v1.b;
			var newFunctions = A3(
				$elm$core$Dict$foldl,
				F3(
					function (n, _v6, newDict) {
						var _v7 = A2($elm$core$Dict$get, n, newDict);
						if (_v7.$ === 1) {
							return newDict;
						} else {
							var _v8 = _v7.a;
							var props = _v8.a;
							var i = _v8.b;
							return (i < 2) ? A2($elm$core$Dict$remove, n, newDict) : A3(
								$elm$core$Dict$insert,
								n,
								_Utils_Tuple2(props, i - 1),
								newDict);
						}
					}),
				model.y,
				topic.y);
			var newConstants = A3(
				$elm$core$Dict$foldl,
				F3(
					function (n, _v3, newSet) {
						var _v4 = A2($elm$core$Dict$get, n, newSet);
						if (_v4.$ === 1) {
							return newSet;
						} else {
							var _v5 = _v4.a;
							var js = _v5.a;
							var i = _v5.b;
							return (i < 2) ? A2($elm$core$Dict$remove, n, newSet) : A3(
								$elm$core$Dict$insert,
								n,
								_Utils_Tuple2(js, i - 1),
								newSet);
						}
					}),
				model.C,
				topic.C);
			return _Utils_update(
				model,
				{
					C: newConstants,
					y: newFunctions,
					v: function () {
						if (url.$ === 1) {
							return A2($elm$core$Dict$remove, name, model.v);
						} else {
							var existing = url.a;
							return A3(
								$elm$core$Dict$insert,
								name,
								$author$project$Components$Rules$NotInstalled_(existing),
								model.v);
						}
					}()
				});
		} else {
			return model;
		}
	});
var $author$project$Helper$resultDict = F2(
	function (process, start) {
		return A2(
			$elm$core$Dict$foldl,
			F3(
				function (key, value, result) {
					return A2(
						$elm$core$Result$andThen,
						A2(process, key, value),
						result);
				}),
			$elm$core$Result$Ok(start));
	});
var $author$project$Components$Rules$addTopic = F2(
	function (topic, m) {
		var model = A2($author$project$Components$Rules$deleteTopic, topic.a, m);
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
								C: constants,
								y: functions,
								v: function () {
									var _v5 = A2($elm$core$Dict$get, topic.a, model.v);
									if (_v5.$ === 1) {
										return A3(
											$elm$core$Dict$insert,
											topic.a,
											A2($author$project$Components$Rules$Installed_, $elm$core$Maybe$Nothing, topic),
											model.v);
									} else {
										if (!_v5.a.$) {
											var url = _v5.a.a;
											return A3(
												$elm$core$Dict$insert,
												topic.a,
												A2(
													$author$project$Components$Rules$Installed_,
													$elm$core$Maybe$Just(url),
													topic),
												model.v);
										} else {
											var _v6 = _v5.a;
											var url = _v6.a;
											return A3(
												$elm$core$Dict$insert,
												topic.a,
												A2($author$project$Components$Rules$Installed_, url, topic),
												model.v);
										}
									}
								}()
							});
					},
					A3(
						$author$project$Helper$resultDict,
						F3(
							function (name, js, dict) {
								var _v3 = A2($elm$core$Dict$get, name, dict);
								if (_v3.$ === 1) {
									return $elm$core$Result$Ok(
										A3(
											$elm$core$Dict$insert,
											name,
											_Utils_Tuple2(js, 1),
											dict));
								} else {
									var _v4 = _v3.a;
									var prev = _v4.a;
									var count = _v4.b;
									return (!_Utils_eq(prev, js)) ? $elm$core$Result$Err(name + ' has different javascript values') : $elm$core$Result$Ok(
										A3(
											$elm$core$Dict$insert,
											name,
											_Utils_Tuple2(prev, count + 1),
											dict));
								}
							}),
						model.C,
						topic.C));
			}
		}(
			A3(
				$author$project$Helper$resultDict,
				F3(
					function (name, props, dict) {
						var _v0 = A2($elm$core$Dict$get, name, dict);
						if (_v0.$ === 1) {
							return $elm$core$Result$Ok(
								A3(
									$elm$core$Dict$insert,
									name,
									_Utils_Tuple2(props, 1),
									dict));
						} else {
							var _v1 = _v0.a;
							var p = _v1.a;
							var count = _v1.b;
							return (_Utils_eq(p.k.e, props.k.e) && (_Utils_eq(p.k.j, props.k.j) && _Utils_eq(p.k.g, props.k.g))) ? $elm$core$Result$Ok(
								A3(
									$elm$core$Dict$insert,
									name,
									_Utils_Tuple2(p, count + 1),
									dict)) : $elm$core$Result$Err('\'' + (name + '\' differs from existing definition from other topics'));
						}
					}),
				model.y,
				topic.y));
	});
var $author$project$Main$submitNotification_ = F2(
	function (model, str) {
		var swappable = model.H;
		var _v0 = A2(
			$author$project$UI$Notification$displayError,
			str,
			_Utils_Tuple2(swappable.aa, $elm$core$Platform$Cmd$none));
		var nModel = _v0.a;
		var nCmd = _v0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					H: _Utils_update(
						swappable,
						{aa: nModel})
				}),
			A2($elm$core$Platform$Cmd$map, $author$project$Main$NotificationEvent, nCmd));
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Algo$History$currentNode_ = function (model) {
	return A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$List$head(model.I));
};
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === -2) {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$sortWith = _List_sortWith;
var $author$project$Algo$History$updateNode_ = F2(
	function (child, n) {
		return function (newChildren) {
			return function (r) {
				return _Utils_Tuple3(
					r.F,
					r.J,
					_Utils_update(
						n,
						{af: newChildren}));
			}(
				A3(
					$elm$core$List$foldl,
					F2(
						function (elem, res) {
							return _Utils_update(
								res,
								{
									F: A2($elm$core$Basics$max, res.F, elem.F),
									J: res.J + elem.J
								});
						}),
					{F: 1, J: 0},
					newChildren));
		}(
			A2(
				$elm$core$List$sortWith,
				F2(
					function (left, right) {
						return (!_Utils_eq(left.F, right.F)) ? ((_Utils_cmp(left.F, right.F) < 0) ? 0 : 2) : ((!_Utils_eq(left.J, right.J)) ? ((_Utils_cmp(left.J, right.J) < 0) ? 0 : 2) : 1);
					}),
				A2(
					$elm$core$List$cons,
					child,
					A2(
						$elm$core$List$filter,
						function (elem) {
							return !_Utils_eq(elem._, child._);
						},
						n.af))));
	});
var $author$project$Algo$History$updateNodeValues_ = F3(
	function (index, child, model) {
		updateNodeValues_:
		while (true) {
			if (!index) {
				var _v0 = A2($author$project$Algo$History$updateNode_, child, model.ar);
				var newNode = _v0.c;
				return _Utils_update(
					model,
					{ar: newNode});
			} else {
				var _v1 = A2($elm$core$Dict$get, index, model.R);
				if (_v1.$ === 1) {
					return model;
				} else {
					var n = _v1.a;
					var _v2 = A2($author$project$Algo$History$updateNode_, child, n);
					var height = _v2.a;
					var width = _v2.b;
					var newNode = _v2.c;
					var $temp$index = n.aJ,
						$temp$child = {F: height, _: index, J: width},
						$temp$model = _Utils_update(
						model,
						{
							R: A3($elm$core$Dict$insert, index, newNode, model.R)
						});
					index = $temp$index;
					child = $temp$child;
					model = $temp$model;
					continue updateNodeValues_;
				}
			}
		}
	});
var $author$project$Algo$History$add = F2(
	function (c, model) {
		var id = $author$project$Algo$History$currentNode_(model);
		var nextID = $elm$core$Dict$size(model.R) + 1;
		return function (newModel) {
			return _Utils_update(
				newModel,
				{
					ad: _List_Nil,
					I: A2($elm$core$List$cons, nextID, model.I)
				});
		}(
			function (nodes) {
				return A3(
					$author$project$Algo$History$updateNodeValues_,
					id,
					{F: 1, _: nextID, J: 1},
					_Utils_update(
						model,
						{R: nodes}));
			}(
				A3(
					$elm$core$Dict$insert,
					nextID,
					{af: _List_Nil, at: c, aJ: id},
					model.R)));
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Algo$History$current = function (model) {
	return A2(
		$elm$core$Maybe$withDefault,
		model.ar,
		A2(
			$elm$core$Maybe$andThen,
			function (n) {
				return A2($elm$core$Dict$get, n, model.R);
			},
			$elm$core$List$head(model.I))).at;
};
var $author$project$Algo$Matcher$affectedSubtree_ = F2(
	function (nodes, parent) {
		var _v0 = $elm$core$Set$toList(nodes);
		if (!_v0.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var start = _v0.a;
			var others = _v0.b;
			var increment = F2(
				function (id, dict) {
					var _v5 = A2($elm$core$Dict$get, id, dict);
					if (_v5.$ === 1) {
						return A3($elm$core$Dict$insert, id, 1, dict);
					} else {
						var c = _v5.a;
						return A3($elm$core$Dict$insert, id, c + 1, dict);
					}
				});
			var count = F2(
				function (id, parentCount) {
					var _v1 = A2($elm$core$Dict$get, id, parent);
					if (_v1.$ === 1) {
						return A2(increment, id, parentCount);
					} else {
						var p = _v1.a;
						return A2(
							increment,
							id,
							A2(count, p, parentCount));
					}
				});
			return function (_v4) {
				var root = _v4.a;
				var newDict = _v4.b;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(
						root,
						$elm$core$Set$fromList(
							$elm$core$Dict$keys(newDict))));
			}(
				function (countDict) {
					var target = $elm$core$List$length(others) + 1;
					var search = F2(
						function (node, dict) {
							var newDict = A2(increment, node, dict);
							return function (r) {
								if (!r.$) {
									var n = r.a;
									return _Utils_Tuple2(n, newDict);
								} else {
									var _v3 = A2($elm$core$Dict$get, node, parent);
									if (_v3.$ === 1) {
										return _Utils_Tuple2(node, newDict);
									} else {
										var p = _v3.a;
										return A2(search, p, newDict);
									}
								}
							}(
								A2(
									$elm$core$Maybe$andThen,
									function (c) {
										return _Utils_eq(c, target) ? $elm$core$Maybe$Just(node) : $elm$core$Maybe$Nothing;
									},
									A2($elm$core$Dict$get, node, newDict)));
						});
					return A2(search, start, countDict);
				}(
					A3($elm$core$List$foldl, count, $elm$core$Dict$empty, others)));
		}
	});
var $author$project$Algo$Matcher$AsIsValue_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Algo$Matcher$MultiNodeReplaced_ = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$Algo$Matcher$SingleNodeReplaced_ = function (a) {
	return {$: 0, a: a};
};
var $author$project$Algo$Math$getState = function (node) {
	switch (node.$) {
		case 0:
			var s = node.a;
			return s.b;
		case 1:
			var s = node.a;
			return s.b;
		case 2:
			var s = node.a;
			return s.b;
		case 3:
			var s = node.a;
			return s.b;
		case 4:
			var s = node.a;
			return s.b;
		default:
			var s = node.a;
			return s.b;
	}
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
var $author$project$Algo$Matcher$duplicateTree_ = F3(
	function (parent, tracker, root) {
		return function (_v0) {
			var finalRoot = _v0.a;
			var finalT = _v0.b;
			return _Utils_Tuple2(
				finalRoot,
				_Utils_update(
					finalT,
					{
						aJ: A3(
							$elm$core$Dict$insert,
							$author$project$Algo$Matcher$getID(
								$author$project$Algo$Math$getState(finalRoot)),
							parent,
							finalT.aJ)
					}));
		}(
			A3(
				$author$project$Algo$Math$map,
				F3(
					function (p, s, t) {
						return A3(
							$author$project$Algo$Matcher$addNode_,
							$elm$core$Maybe$Just(
								$author$project$Algo$Math$getState(s)),
							A2(
								$elm$core$Maybe$withDefault,
								parent,
								A2($elm$core$Maybe$map, $author$project$Algo$Matcher$getID, p)),
							t);
					}),
				tracker,
				root));
	});
var $author$project$Algo$Matcher$replacedToTree_ = F3(
	function (p, t, r) {
		if (!r.$) {
			var tree = r.a;
			return _Utils_Tuple2(tree, t);
		} else {
			var f = r.a;
			var pre = r.b;
			var post = r.c;
			var _v1 = A3($author$project$Algo$Matcher$addNode_, $elm$core$Maybe$Nothing, p, t);
			var st = _v1.a;
			var newT = _v1.b;
			return _Utils_Tuple2(
				$author$project$Algo$Math$BinaryNode(
					{
						j: f.j,
						af: _Utils_ap(pre, post),
						g: f.g,
						a: f.a,
						b: st
					}),
				_Utils_update(
					newT,
					{
						aJ: A3(
							$elm$core$List$foldl,
							function (child) {
								return A2(
									$elm$core$Dict$insert,
									$author$project$Algo$Matcher$getID(
										$author$project$Algo$Math$getState(child)),
									t.o);
							},
							newT.aJ,
							_Utils_ap(pre, post))
					}));
		}
	});
var $author$project$Algo$Matcher$FList_ = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$Algo$Matcher$RList_ = function (a) {
	return {$: 0, a: a};
};
var $author$project$Algo$Matcher$toReplacementList_ = F3(
	function (parent, name, tracker) {
		return A2(
			$elm$core$Basics$composeR,
			A2(
				$elm$core$List$foldl,
				F2(
					function (child, _v0) {
						var list = _v0.a;
						var oT = _v0.b;
						var _v1 = _Utils_Tuple2(list, child);
						if (_v1.a.$ === 1) {
							if (!_v1.b.$) {
								var _v2 = _v1.a;
								var tree = _v1.b.a;
								return _Utils_Tuple2(
									$elm$core$Maybe$Just(
										$author$project$Algo$Matcher$RList_(
											_List_fromArray(
												[tree]))),
									oT);
							} else {
								var _v3 = _v1.a;
								var _v4 = _v1.b;
								var p = _v4.a;
								var pre = _v4.b;
								var post = _v4.c;
								return _Utils_eq(p.a, name) ? _Utils_Tuple2(
									$elm$core$Maybe$Just(
										A3($author$project$Algo$Matcher$FList_, p, pre, post)),
									oT) : function (_v5) {
									var s = _v5.a;
									var t = _v5.b;
									return _Utils_Tuple2(
										$elm$core$Maybe$Just(
											$author$project$Algo$Matcher$RList_(
												_List_fromArray(
													[
														$author$project$Algo$Math$BinaryNode(
														{
															j: p.j,
															af: _Utils_ap(pre, post),
															g: p.g,
															a: p.a,
															b: s
														})
													]))),
										t);
								}(
									A3($author$project$Algo$Matcher$addNode_, $elm$core$Maybe$Nothing, parent, oT));
							}
						} else {
							if (!_v1.a.a.$) {
								if (!_v1.b.$) {
									var n = _v1.a.a.a;
									var tree = _v1.b.a;
									return _Utils_Tuple2(
										$elm$core$Maybe$Just(
											$author$project$Algo$Matcher$RList_(
												A2($elm$core$List$cons, tree, n))),
										oT);
								} else {
									var n = _v1.a.a.a;
									var _v6 = _v1.b;
									var p = _v6.a;
									var pre = _v6.b;
									var post = _v6.c;
									return _Utils_eq(p.a, name) ? _Utils_Tuple2(
										$elm$core$Maybe$Just(
											A3(
												$author$project$Algo$Matcher$FList_,
												p,
												pre,
												_Utils_ap(post, n))),
										oT) : function (_v7) {
										var s = _v7.a;
										var t = _v7.b;
										return _Utils_Tuple2(
											$elm$core$Maybe$Just(
												$author$project$Algo$Matcher$RList_(
													A2(
														$elm$core$List$cons,
														$author$project$Algo$Math$BinaryNode(
															{
																j: p.j,
																af: _Utils_ap(pre, post),
																g: p.g,
																a: p.a,
																b: s
															}),
														n))),
											t);
									}(
										A3($author$project$Algo$Matcher$addNode_, $elm$core$Maybe$Nothing, parent, oT));
								}
							} else {
								if (!_v1.b.$) {
									var _v8 = _v1.a.a;
									var n = _v8.a;
									var preOp = _v8.b;
									var postOp = _v8.c;
									var tree = _v1.b.a;
									return _Utils_Tuple2(
										$elm$core$Maybe$Just(
											A3(
												$author$project$Algo$Matcher$FList_,
												n,
												preOp,
												A2($elm$core$List$cons, tree, postOp))),
										oT);
								} else {
									var _v9 = _v1.a.a;
									var n = _v9.a;
									var preOp = _v9.b;
									var postOp = _v9.c;
									var _v10 = _v1.b;
									var p = _v10.a;
									var pre = _v10.b;
									var post = _v10.c;
									return _Utils_eq(p.a, name) ? _Utils_Tuple2(
										$elm$core$Maybe$Just(
											A3(
												$author$project$Algo$Matcher$FList_,
												n,
												preOp,
												_Utils_ap(
													pre,
													_Utils_ap(post, postOp)))),
										oT) : function (_v11) {
										var s = _v11.a;
										var t = _v11.b;
										return _Utils_Tuple2(
											$elm$core$Maybe$Just(
												A3(
													$author$project$Algo$Matcher$FList_,
													n,
													preOp,
													A2(
														$elm$core$List$cons,
														$author$project$Algo$Math$BinaryNode(
															{
																j: p.j,
																af: _Utils_ap(pre, post),
																g: p.g,
																a: p.a,
																b: s
															}),
														postOp))),
											t);
									}(
										A3($author$project$Algo$Matcher$addNode_, $elm$core$Maybe$Nothing, parent, oT));
								}
							}
						}
					}),
				_Utils_Tuple2($elm$core$Maybe$Nothing, tracker)),
			function (_v12) {
				var replacement = _v12.a;
				var t = _v12.b;
				if (replacement.$ === 1) {
					return $elm$core$Result$Err('Missing children');
				} else {
					var r = replacement.a;
					return $elm$core$Result$Ok(
						_Utils_Tuple2(r, t));
				}
			});
	});
var $author$project$Algo$Matcher$constructFromInternalReplacement_ = F4(
	function (parent, tracker, _arguments, replaceNode) {
		var id = tracker.o;
		var constructChildren = F2(
			function (name, firstT) {
				return A2(
					$elm$core$Basics$composeR,
					A2(
						$author$project$Helper$resultList,
						F2(
							function (child, _v53) {
								var list = _v53.a;
								var oT = _v53.b;
								var args = _v53.c;
								return A2(
									$elm$core$Result$map,
									function (_v54) {
										var newChild = _v54.a;
										var newT = _v54.b;
										var newRes = _v54.c;
										return _Utils_Tuple3(
											A2($elm$core$List$cons, newChild, list),
											newT,
											newRes);
									},
									A4($author$project$Algo$Matcher$constructFromInternalReplacement_, id, oT, args, child));
							}),
						_Utils_Tuple3(_List_Nil, firstT, _arguments)),
					$elm$core$Result$andThen(
						function (_v55) {
							var children = _v55.a;
							var newT = _v55.b;
							var a = _v55.c;
							return A2(
								$elm$core$Result$map,
								function (_v56) {
									var rList = _v56.a;
									var finalT = _v56.b;
									return _Utils_Tuple3(rList, finalT, a);
								},
								A4($author$project$Algo$Matcher$toReplacementList_, id, name, newT, children));
						}));
			});
		var _v32 = A3(
			$author$project$Algo$Matcher$addNode_,
			$elm$core$Maybe$Just(
				$author$project$Algo$Math$getState(replaceNode).a),
			parent,
			tracker);
		var s = _v32.a;
		var t = _v32.b;
		switch (replaceNode.$) {
			case 0:
				var n = replaceNode.a;
				return $elm$core$Result$Ok(
					_Utils_Tuple3(
						$author$project$Algo$Matcher$SingleNodeReplaced_(
							$author$project$Algo$Math$RealNode(
								{b: s, x: n.x})),
						t,
						_arguments));
			case 2:
				var n = replaceNode.a;
				return A2(
					$elm$core$Result$map,
					function (_v34) {
						var newChild = _v34.a;
						var newT = _v34.b;
						var newArg = _v34.c;
						return function (_v35) {
							var tree = _v35.a;
							var finalT = _v35.b;
							return _Utils_Tuple3(
								$author$project$Algo$Matcher$SingleNodeReplaced_(
									$author$project$Algo$Math$UnaryNode(
										{N: tree, a: n.a, b: s})),
								finalT,
								newArg);
						}(
							A3($author$project$Algo$Matcher$replacedToTree_, id, newT, newChild));
					},
					A4($author$project$Algo$Matcher$constructFromInternalReplacement_, id, t, _arguments, n.N));
			case 3:
				var n = replaceNode.a;
				return A2(
					$elm$core$Result$map,
					function (_v36) {
						var res = _v36.a;
						var newT = _v36.b;
						var newArg = _v36.c;
						return _Utils_Tuple3(
							function () {
								if (!res.$) {
									var children = res.a;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$BinaryNode(
											{j: n.j, af: children, g: n.g, a: n.a, b: s}));
								} else {
									var pre = res.b;
									var post = res.c;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$BinaryNode(
											{
												j: n.j,
												af: _Utils_ap(pre, post),
												g: n.g,
												a: n.a,
												b: s
											}));
								}
							}(),
							newT,
							newArg);
					},
					A3(constructChildren, n.a, t, n.af));
			case 5:
				var n = replaceNode.a;
				return A2(
					$elm$core$Result$map,
					function (_v38) {
						var res = _v38.a;
						var newT = _v38.b;
						var newArg = _v38.c;
						return _Utils_Tuple3(
							function () {
								if (!res.$) {
									var children = res.a;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$DeclarativeNode(
											{af: children, a: n.a, b: s}));
								} else {
									var pre = res.b;
									var post = res.c;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$DeclarativeNode(
											{
												af: _Utils_ap(pre, post),
												a: n.a,
												b: s
											}));
								}
							}(),
							newT,
							newArg);
					},
					A3(constructChildren, n.a, t, n.af));
			case 1:
				var n = replaceNode.a;
				var _v40 = n.b;
				if (_v40.b.$ === 1) {
					var oldS = _v40.a;
					var _v41 = _v40.b;
					var newState = A2(tracker.aF, oldS, tracker.o);
					return $elm$core$Result$Ok(
						_Utils_Tuple3(
							$author$project$Algo$Matcher$SingleNodeReplaced_(
								$author$project$Algo$Math$VariableNode(
									{
										a: n.a,
										b: A2($author$project$Algo$Matcher$State_, tracker.o, newState)
									})),
							_Utils_update(
								tracker,
								{
									o: tracker.o + 1,
									aJ: A3($elm$core$Dict$insert, tracker.o, parent, tracker.aJ)
								}),
							_arguments));
				} else {
					var argNum = _v40.b.a;
					var _v42 = A2($elm$core$Dict$get, argNum, _arguments);
					if (_v42.$ === 1) {
						return $elm$core$Result$Err('Couldn\'t find value to replace');
					} else {
						var value = _v42.a;
						return A2(
							$elm$core$Result$map,
							function (_v43) {
								var replaced = _v43.a;
								var newT = _v43.b;
								var newVal = _v43.c;
								return function (_v44) {
									var tree = _v44.a;
									var finalT = _v44.b;
									return _Utils_Tuple3(
										$author$project$Algo$Matcher$SingleNodeReplaced_(tree),
										finalT,
										A3($elm$core$Dict$insert, argNum, newVal, _arguments));
								}(
									A3($author$project$Algo$Matcher$replacedToTree_, parent, newT, replaced));
							},
							A4($author$project$Algo$Matcher$constructFromValue_, parent, tracker, $elm$core$Dict$empty, value));
					}
				}
			default:
				var n = replaceNode.a;
				var _v45 = n.b;
				if (_v45.b.$ === 1) {
					var oldS = _v45.a;
					var _v46 = _v45.b;
					return A2(
						$elm$core$Result$map,
						function (_v47) {
							var res = _v47.a;
							var newT = _v47.b;
							var newArg = _v47.c;
							return _Utils_Tuple3(
								function () {
									if (!res.$) {
										var children = res.a;
										return $author$project$Algo$Matcher$SingleNodeReplaced_(
											$author$project$Algo$Math$GenericNode(
												{af: children, a: n.a, b: s}));
									} else {
										var pre = res.b;
										var post = res.c;
										return $author$project$Algo$Matcher$SingleNodeReplaced_(
											$author$project$Algo$Math$GenericNode(
												{
													af: _Utils_ap(pre, post),
													a: n.a,
													b: s
												}));
									}
								}(),
								newT,
								newArg);
						},
						A3(constructChildren, '', t, n.af));
				} else {
					var argNum = _v45.b.a;
					var _v49 = A2($elm$core$Dict$get, argNum, _arguments);
					if (_v49.$ === 1) {
						return $elm$core$Result$Err('Couldn\'t find the function to replace');
					} else {
						var value = _v49.a;
						return A2(
							$elm$core$Result$andThen,
							function (_v50) {
								var res = _v50.a;
								var newT = _v50.b;
								var newArg = _v50.c;
								if (res.$ === 1) {
									return $elm$core$Result$Err('Something went wrong: Grouping detected');
								} else {
									var children = res.a;
									return function (childArgs) {
										return A2(
											$elm$core$Result$map,
											function (_v52) {
												var finalNode = _v52.a;
												var finalT = _v52.b;
												var newVal = _v52.c;
												return _Utils_Tuple3(
													finalNode,
													finalT,
													A3($elm$core$Dict$insert, argNum, newVal, newArg));
											},
											A4(
												$author$project$Algo$Matcher$constructFromValue_,
												parent,
												newT,
												$elm$core$Dict$fromList(childArgs),
												value));
									}(
										A2(
											$elm$core$List$indexedMap,
											F2(
												function (num, child) {
													return _Utils_Tuple2(
														num,
														A2($author$project$Algo$Matcher$AsIsValue_, false, child));
												}),
											children));
								}
							},
							A3(constructChildren, '', tracker, n.af));
					}
				}
		}
	});
var $author$project$Algo$Matcher$constructFromReplacement_ = F4(
	function (parent, tracker, _arguments, replaceNode) {
		var id = tracker.o;
		var constructChildren = F2(
			function (name, firstT) {
				return A2(
					$elm$core$Basics$composeR,
					A2(
						$author$project$Helper$resultList,
						F2(
							function (child, _v28) {
								var list = _v28.a;
								var oT = _v28.b;
								var args = _v28.c;
								return A2(
									$elm$core$Result$map,
									function (_v29) {
										var newChild = _v29.a;
										var newT = _v29.b;
										var newRes = _v29.c;
										return _Utils_Tuple3(
											A2($elm$core$List$cons, newChild, list),
											newT,
											newRes);
									},
									A4($author$project$Algo$Matcher$constructFromReplacement_, id, oT, args, child));
							}),
						_Utils_Tuple3(_List_Nil, firstT, _arguments)),
					$elm$core$Result$andThen(
						function (_v30) {
							var children = _v30.a;
							var newT = _v30.b;
							var a = _v30.c;
							return A2(
								$elm$core$Result$map,
								function (_v31) {
									var rList = _v31.a;
									var finalT = _v31.b;
									return _Utils_Tuple3(rList, finalT, a);
								},
								A4($author$project$Algo$Matcher$toReplacementList_, id, name, newT, children));
						}));
			});
		var _v9 = A3($author$project$Algo$Matcher$addNode_, $elm$core$Maybe$Nothing, parent, tracker);
		var s = _v9.a;
		var t = _v9.b;
		switch (replaceNode.$) {
			case 0:
				var n = replaceNode.a;
				return $elm$core$Result$Ok(
					_Utils_Tuple3(
						$author$project$Algo$Matcher$SingleNodeReplaced_(
							$author$project$Algo$Math$RealNode(
								{b: s, x: n.x})),
						t,
						_arguments));
			case 2:
				var n = replaceNode.a;
				return A2(
					$elm$core$Result$map,
					function (_v11) {
						var newChild = _v11.a;
						var newT = _v11.b;
						var newArg = _v11.c;
						return function (_v12) {
							var tree = _v12.a;
							var finalT = _v12.b;
							return _Utils_Tuple3(
								$author$project$Algo$Matcher$SingleNodeReplaced_(
									$author$project$Algo$Math$UnaryNode(
										{N: tree, a: n.a, b: s})),
								finalT,
								newArg);
						}(
							A3($author$project$Algo$Matcher$replacedToTree_, id, newT, newChild));
					},
					A4($author$project$Algo$Matcher$constructFromReplacement_, id, t, _arguments, n.N));
			case 3:
				var n = replaceNode.a;
				return A2(
					$elm$core$Result$map,
					function (_v13) {
						var res = _v13.a;
						var newT = _v13.b;
						var newArg = _v13.c;
						return _Utils_Tuple3(
							function () {
								if (!res.$) {
									var children = res.a;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$BinaryNode(
											{j: n.j, af: children, g: n.g, a: n.a, b: s}));
								} else {
									var pre = res.b;
									var post = res.c;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$BinaryNode(
											{
												j: n.j,
												af: _Utils_ap(pre, post),
												g: n.g,
												a: n.a,
												b: s
											}));
								}
							}(),
							newT,
							newArg);
					},
					A3(constructChildren, n.a, t, n.af));
			case 5:
				var n = replaceNode.a;
				return A2(
					$elm$core$Result$map,
					function (_v15) {
						var res = _v15.a;
						var newT = _v15.b;
						var newArg = _v15.c;
						return _Utils_Tuple3(
							function () {
								if (!res.$) {
									var children = res.a;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$DeclarativeNode(
											{af: children, a: n.a, b: s}));
								} else {
									var pre = res.b;
									var post = res.c;
									return $author$project$Algo$Matcher$SingleNodeReplaced_(
										$author$project$Algo$Math$DeclarativeNode(
											{
												af: _Utils_ap(pre, post),
												a: n.a,
												b: s
											}));
								}
							}(),
							newT,
							newArg);
					},
					A3(constructChildren, n.a, t, n.af));
			case 1:
				var n = replaceNode.a;
				var _v17 = n.b;
				if (_v17.$ === 1) {
					return $elm$core$Result$Ok(
						_Utils_Tuple3(
							$author$project$Algo$Matcher$SingleNodeReplaced_(
								$author$project$Algo$Math$VariableNode(
									{a: n.a, b: s})),
							t,
							_arguments));
				} else {
					var argNum = _v17.a;
					var _v18 = A2($elm$core$Dict$get, argNum, _arguments);
					if (_v18.$ === 1) {
						return $elm$core$Result$Err('Couldn\'t find value to replace');
					} else {
						var value = _v18.a;
						return A2(
							$elm$core$Result$map,
							function (_v19) {
								var replaced = _v19.a;
								var newT = _v19.b;
								var val = _v19.c;
								return function (_v20) {
									var tree = _v20.a;
									var finalT = _v20.b;
									return _Utils_Tuple3(
										$author$project$Algo$Matcher$SingleNodeReplaced_(tree),
										finalT,
										A3($elm$core$Dict$insert, argNum, val, _arguments));
								}(
									A3($author$project$Algo$Matcher$replacedToTree_, parent, newT, replaced));
							},
							A4($author$project$Algo$Matcher$constructFromValue_, parent, tracker, $elm$core$Dict$empty, value));
					}
				}
			default:
				var n = replaceNode.a;
				var _v21 = n.b;
				if (_v21.$ === 1) {
					return A2(
						$elm$core$Result$map,
						function (_v22) {
							var res = _v22.a;
							var newT = _v22.b;
							var newArg = _v22.c;
							return _Utils_Tuple3(
								function () {
									if (!res.$) {
										var children = res.a;
										return $author$project$Algo$Matcher$SingleNodeReplaced_(
											$author$project$Algo$Math$GenericNode(
												{af: children, a: n.a, b: s}));
									} else {
										var pre = res.b;
										var post = res.c;
										return $author$project$Algo$Matcher$SingleNodeReplaced_(
											$author$project$Algo$Math$GenericNode(
												{
													af: _Utils_ap(pre, post),
													a: n.a,
													b: s
												}));
									}
								}(),
								newT,
								newArg);
						},
						A3(constructChildren, '', t, n.af));
				} else {
					var argNum = _v21.a;
					var _v24 = A2($elm$core$Dict$get, argNum, _arguments);
					if (_v24.$ === 1) {
						return $elm$core$Result$Err('Couldn\'t find the function to replace');
					} else {
						var value = _v24.a;
						return A2(
							$elm$core$Result$andThen,
							function (_v25) {
								var res = _v25.a;
								var newT = _v25.b;
								var newArg = _v25.c;
								if (res.$ === 1) {
									return $elm$core$Result$Err('Something went wrong: Grouping detected');
								} else {
									var children = res.a;
									return function (childArgs) {
										return A2(
											$elm$core$Result$map,
											function (_v27) {
												var finalNode = _v27.a;
												var finalT = _v27.b;
												var newVal = _v27.c;
												return _Utils_Tuple3(
													finalNode,
													finalT,
													A3($elm$core$Dict$insert, argNum, newVal, newArg));
											},
											A4(
												$author$project$Algo$Matcher$constructFromValue_,
												parent,
												newT,
												$elm$core$Dict$fromList(childArgs),
												value));
									}(
										A2(
											$elm$core$List$indexedMap,
											F2(
												function (num, child) {
													return _Utils_Tuple2(
														num,
														A2($author$project$Algo$Matcher$AsIsValue_, false, child));
												}),
											children));
								}
							},
							A3(constructChildren, '', tracker, n.af));
					}
				}
		}
	});
var $author$project$Algo$Matcher$constructFromValue_ = F4(
	function (parent, tracker, args, value) {
		switch (value.$) {
			case 0:
				var r = value.a;
				return A2(
					$elm$core$Result$map,
					function (_v1) {
						var newChild = _v1.a;
						var newT = _v1.b;
						var newArgs = _v1.c;
						return _Utils_Tuple3(newChild, newT, value);
					},
					A4($author$project$Algo$Matcher$constructFromReplacement_, parent, tracker, args, r));
			case 1:
				var r = value.a;
				return A2(
					$elm$core$Result$map,
					function (_v2) {
						var newChild = _v2.a;
						var newT = _v2.b;
						var newArgs = _v2.c;
						return _Utils_Tuple3(newChild, newT, value);
					},
					A4($author$project$Algo$Matcher$constructFromInternalReplacement_, parent, tracker, args, r));
			case 2:
				var p = value.a;
				var processChildren = F2(
					function (internalRep, _v7) {
						var list = _v7.a;
						var t = _v7.b;
						var innerArgs = _v7.c;
						return A2(
							$elm$core$Result$map,
							function (_v5) {
								var replaced = _v5.a;
								var newT = _v5.b;
								var newArgs = _v5.c;
								return function (_v6) {
									var tree = _v6.a;
									var finalT = _v6.b;
									return _Utils_Tuple3(
										A2($elm$core$List$cons, tree, list),
										finalT,
										newArgs);
								}(
									A3($author$project$Algo$Matcher$replacedToTree_, parent, newT, replaced));
							},
							A4($author$project$Algo$Matcher$constructFromInternalReplacement_, parent, t, innerArgs, internalRep));
					});
				return A2(
					$elm$core$Result$andThen,
					function (_v3) {
						var newPre = _v3.a;
						var nextT = _v3.b;
						var nextArgs = _v3.c;
						return A2(
							$elm$core$Result$map,
							function (_v4) {
								var newPost = _v4.a;
								var finalT = _v4.b;
								return _Utils_Tuple3(
									A3(
										$author$project$Algo$Matcher$MultiNodeReplaced_,
										{j: p.j, g: p.g, a: p.bG},
										$elm$core$List$reverse(newPre),
										$elm$core$List$reverse(newPost)),
									finalT,
									value);
							},
							A3(
								$author$project$Helper$resultList,
								processChildren,
								_Utils_Tuple3(_List_Nil, nextT, nextArgs),
								p.bM));
					},
					A3(
						$author$project$Helper$resultList,
						processChildren,
						_Utils_Tuple3(_List_Nil, tracker, args),
						p.bN));
			default:
				var used = value.a;
				var tree = value.b;
				return used ? function (_v8) {
					var newRoot = _v8.a;
					var newTree = _v8.b;
					return $elm$core$Result$Ok(
						_Utils_Tuple3(
							$author$project$Algo$Matcher$SingleNodeReplaced_(newRoot),
							newTree,
							value));
				}(
					A3($author$project$Algo$Matcher$duplicateTree_, parent, tracker, tree)) : $elm$core$Result$Ok(
					_Utils_Tuple3(
						$author$project$Algo$Matcher$SingleNodeReplaced_(tree),
						_Utils_update(
							tracker,
							{
								aJ: A3(
									$elm$core$Dict$insert,
									$author$project$Algo$Matcher$getID(
										$author$project$Algo$Math$getState(tree)),
									parent,
									tracker.aJ)
							}),
						A2($author$project$Algo$Matcher$AsIsValue_, true, tree)));
		}
	});
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $elm$core$Set$diff = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$diff, dict1, dict2);
	});
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
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $elm$core$Set$singleton = function (key) {
	return A2($elm$core$Dict$singleton, key, 0);
};
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$union, dict1, dict2);
	});
var $author$project$Algo$Matcher$getSubtreeIDs_ = function (root) {
	switch (root.$) {
		case 0:
			var s = root.a;
			return $elm$core$Set$singleton(
				$author$project$Algo$Matcher$getID(s.b));
		case 1:
			var s = root.a;
			return $elm$core$Set$singleton(
				$author$project$Algo$Matcher$getID(s.b));
		case 2:
			var s = root.a;
			return A2(
				$elm$core$Set$insert,
				$author$project$Algo$Matcher$getID(s.b),
				$author$project$Algo$Matcher$getSubtreeIDs_(s.N));
		case 3:
			var s = root.a;
			return A3(
				$elm$core$List$foldl,
				function (e) {
					return $elm$core$Set$union(
						$author$project$Algo$Matcher$getSubtreeIDs_(e));
				},
				$elm$core$Set$singleton(
					$author$project$Algo$Matcher$getID(s.b)),
				s.af);
		case 4:
			var s = root.a;
			return A3(
				$elm$core$List$foldl,
				function (e) {
					return $elm$core$Set$union(
						$author$project$Algo$Matcher$getSubtreeIDs_(e));
				},
				$elm$core$Set$singleton(
					$author$project$Algo$Matcher$getID(s.b)),
				s.af);
		default:
			var s = root.a;
			return A3(
				$elm$core$List$foldl,
				function (e) {
					return $elm$core$Set$union(
						$author$project$Algo$Matcher$getSubtreeIDs_(e));
				},
				$elm$core$Set$singleton(
					$author$project$Algo$Matcher$getID(s.b)),
				s.af);
	}
};
var $author$project$Algo$Matcher$processSubtree_ = F3(
	function (path, processor, eq) {
		var processChildren = F2(
			function (p, children) {
				if (!children.b) {
					return $elm$core$Result$Err('Children not found');
				} else {
					var current = children.a;
					var others = children.b;
					var _v1 = A3(
						$author$project$Algo$Matcher$processSubtree_,
						p,
						processor,
						_Utils_update(
							eq,
							{ar: current}));
					if (_v1.$ === 1) {
						return A2(
							$elm$core$Result$map,
							function (_v2) {
								var r = _v2.a;
								var newChildren = _v2.b;
								var t = _v2.c;
								return _Utils_Tuple3(
									r,
									A2($elm$core$List$cons, current, newChildren),
									t);
							},
							A2(processChildren, p, others));
					} else {
						var _v3 = _v1.a;
						var a = _v3.a;
						var newEq = _v3.b;
						return $elm$core$Result$Ok(
							_Utils_Tuple3(
								a,
								A2($elm$core$List$cons, newEq.ar, others),
								newEq.m));
					}
				}
			});
		var id = $author$project$Algo$Matcher$getID(
			$author$project$Algo$Math$getState(eq.ar));
		if (!path.b) {
			return $elm$core$Result$Err('Target node not found');
		} else {
			if (!path.b.b) {
				var expected = path.a;
				return (!_Utils_eq(expected, id)) ? $elm$core$Result$Err('Intermediary node not found') : processor(eq);
			} else {
				var expected = path.a;
				var next = path.b;
				if (!_Utils_eq(expected, id)) {
					return $elm$core$Result$Err('Intermediary node not found');
				} else {
					var _v5 = eq.ar;
					switch (_v5.$) {
						case 0:
							return $elm$core$Result$Err('Cannot find children in RealNode');
						case 1:
							return $elm$core$Result$Err('Cannot find children in VariableNode');
						case 2:
							var s = _v5.a;
							return A2(
								$elm$core$Result$map,
								function (_v6) {
									var r = _v6.a;
									var newEq = _v6.b;
									return _Utils_Tuple2(
										r,
										_Utils_update(
											newEq,
											{
												ar: $author$project$Algo$Math$UnaryNode(
													_Utils_update(
														s,
														{N: newEq.ar}))
											}));
								},
								A3(
									$author$project$Algo$Matcher$processSubtree_,
									next,
									processor,
									_Utils_update(
										eq,
										{ar: s.N})));
						case 3:
							var s = _v5.a;
							return A2(
								$elm$core$Result$map,
								function (_v7) {
									var r = _v7.a;
									var children = _v7.b;
									var t = _v7.c;
									return _Utils_Tuple2(
										r,
										{
											ar: $author$project$Algo$Math$BinaryNode(
												_Utils_update(
													s,
													{af: children})),
											m: t
										});
								},
								A2(processChildren, next, s.af));
						case 4:
							var s = _v5.a;
							return A2(
								$elm$core$Result$map,
								function (_v8) {
									var r = _v8.a;
									var children = _v8.b;
									var t = _v8.c;
									return _Utils_Tuple2(
										r,
										{
											ar: $author$project$Algo$Math$GenericNode(
												_Utils_update(
													s,
													{af: children})),
											m: t
										});
								},
								A2(processChildren, next, s.af));
						default:
							var s = _v5.a;
							return A2(
								$elm$core$Result$map,
								function (_v9) {
									var r = _v9.a;
									var children = _v9.b;
									var t = _v9.c;
									return _Utils_Tuple2(
										r,
										{
											ar: $author$project$Algo$Math$DeclarativeNode(
												_Utils_update(
													s,
													{af: children})),
											m: t
										});
								},
								A2(processChildren, next, s.af));
					}
				}
			}
		}
	});
var $author$project$Algo$Matcher$searchPath_ = F2(
	function (map, id) {
		var recursive = function (input) {
			var _v0 = A2($elm$core$Dict$get, input, map);
			if (_v0.$ === 1) {
				return _List_fromArray(
					[input]);
			} else {
				var a = _v0.a;
				return A2(
					$elm$core$List$cons,
					input,
					recursive(a));
			}
		};
		return $elm$core$List$reverse(
			recursive(id));
	});
var $author$project$Algo$Matcher$setParent_ = F3(
	function (root, parent, tracker) {
		var id = $author$project$Algo$Matcher$getID(
			$author$project$Algo$Math$getState(root));
		if (parent.$ === 1) {
			return _Utils_update(
				tracker,
				{
					aJ: A2($elm$core$Dict$remove, id, tracker.aJ)
				});
		} else {
			var p = parent.a;
			return _Utils_update(
				tracker,
				{
					aJ: A3($elm$core$Dict$insert, id, p, tracker.aJ)
				});
		}
	});
var $author$project$Algo$Matcher$replaceSubtree = F4(
	function (ids, replacement, _with, eq) {
		var _v0 = A2($author$project$Algo$Matcher$affectedSubtree_, ids, eq.m.aJ);
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('Unable to find selected nodes');
		} else {
			var _v1 = _v0.a;
			var id = _v1.a;
			return A3(
				$author$project$Algo$Matcher$processSubtree_,
				A2($author$project$Algo$Matcher$searchPath_, eq.m.aJ, id),
				function (subEq) {
					var args = $elm$core$Dict$fromList(
						A2(
							$elm$core$List$indexedMap,
							F2(
								function (index, _v4) {
									var val = _v4.b;
									return _Utils_Tuple2(index, val);
								}),
							$elm$core$Dict$toList(_with)));
					return A2(
						$elm$core$Result$map,
						function (_v2) {
							var r = _v2.a;
							var t = _v2.b;
							var pID = A2(
								$elm$core$Dict$get,
								$author$project$Algo$Matcher$getID(
									$author$project$Algo$Math$getState(subEq.ar)),
								subEq.m.aJ);
							var _v3 = A3($author$project$Algo$Matcher$replacedToTree_, 0, t, r);
							var newRoot = _v3.a;
							var newTracker = _v3.b;
							var finalTracker = A3(
								$elm$core$Set$foldl,
								F2(
									function (elem, childT) {
										return _Utils_update(
											childT,
											{
												aJ: A2($elm$core$Dict$remove, elem, childT.aJ)
											});
									}),
								A3($author$project$Algo$Matcher$setParent_, newRoot, pID, newTracker),
								A2(
									$elm$core$Set$diff,
									$author$project$Algo$Matcher$getSubtreeIDs_(subEq.ar),
									$author$project$Algo$Matcher$getSubtreeIDs_(newRoot)));
							return _Utils_Tuple2(
								$author$project$Algo$Matcher$getID(
									$author$project$Algo$Math$getState(newRoot)),
								{ar: newRoot, m: finalTracker});
						},
						A4($author$project$Algo$Matcher$constructFromReplacement_, -1, subEq.m, args, replacement));
				},
				eq);
		}
	});
var $author$project$Components$Display$transformEquation = F3(
	function (replacement, result, model) {
		var _v0 = model.n;
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('No nodes were selected');
		} else {
			var _v1 = _v0.a;
			var eqNum = _v1.a;
			var ids = _v1.b;
			var _v2 = A2($elm$core$Dict$get, eqNum, model.f);
			if (_v2.$ === 1) {
				return $elm$core$Result$Err('Equation is not found');
			} else {
				var eq = _v2.a;
				return A2(
					$elm$core$Result$map,
					function (_v3) {
						var newSelect = _v3.a;
						var newEq = _v3.b;
						return _Utils_update(
							model,
							{
								f: A3(
									$elm$core$Dict$insert,
									eqNum,
									A2($author$project$Algo$History$add, newEq, eq),
									model.f),
								n: $elm$core$Maybe$Just(
									_Utils_Tuple2(
										eqNum,
										$elm$core$Set$singleton(newSelect)))
							});
					},
					A4(
						$author$project$Algo$Matcher$replaceSubtree,
						ids,
						replacement,
						result,
						$author$project$Algo$History$current(eq)));
			}
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
var $author$project$Components$Display$listEquations = function (model) {
	return A2(
		$elm$core$Dict$map,
		function (_v0) {
			return $author$project$Algo$History$current;
		},
		model.f);
};
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $author$project$Components$Query$marshalEquations_ = A2(
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
		var _v0 = url.bQ;
		if (!_v0) {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.bx,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.bR,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.bL,
					_Utils_ap(http, url.bA)),
				url.bJ)));
};
var $author$project$Components$Query$pushUrl = function (model) {
	var eqs = $author$project$Components$Query$marshalEquations_(model.f);
	return A2(
		$elm$browser$Browser$Navigation$replaceUrl,
		model.a6,
		function (url) {
			return $elm$url$Url$toString(
				_Utils_update(
					url,
					{
						bR: $elm$core$Maybe$Just(eqs)
					}));
		}(model.a1));
};
var $author$project$Algo$Math$eqToString_ = function (root) {
	if (!root.$) {
		var str = root.a;
		return str;
	} else {
		var s = root.a;
		return A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, $author$project$Algo$Math$eqToString_, s.af));
	}
};
var $author$project$Algo$Math$Node = function (a) {
	return {$: 1, a: a};
};
var $author$project$Algo$Math$Text = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Algo$Math$getName = function (root) {
	switch (root.$) {
		case 0:
			var n = root.a;
			return $elm$core$String$fromFloat(n.x);
		case 1:
			var n = root.a;
			return n.a;
		case 2:
			var n = root.a;
			return n.a;
		case 3:
			var n = root.a;
			return n.a;
		case 4:
			var n = root.a;
			return n.a;
		default:
			var n = root.a;
			return n.a;
	}
};
var $author$project$Algo$Math$functionPrecedence_ = function (node) {
	var _v0 = $author$project$Algo$Math$getName(node);
	switch (_v0) {
		case '/':
			return 3;
		case '-':
			return 3;
		case '*':
			return 2;
		case '+':
			return 1;
		default:
			return -1;
	}
};
var $elm$core$Basics$ge = _Utils_ge;
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
var $author$project$Algo$Math$symbolicateRecursive_ = F3(
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
							var s = _v3.b.a;
							return $author$project$Algo$Math$Node(
								{af: tokens, b: s.b});
						}
					case 1:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							var s = _v3.b.a;
							return $author$project$Algo$Math$Node(
								{af: tokens, b: s.b});
						}
					case 4:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							var s = _v3.b.a;
							return $author$project$Algo$Math$Node(
								{af: tokens, b: s.b});
						}
					case 5:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							var s = _v3.b.a;
							return $author$project$Algo$Math$Node(
								{af: tokens, b: s.b});
						}
					default:
						if (_v3.a.$ === 1) {
							break _v3$0;
						} else {
							var p = _v3.a.a;
							return (_Utils_cmp(
								$author$project$Algo$Math$functionPrecedence_(p),
								$author$project$Algo$Math$functionPrecedence_(root)) > -1) ? $author$project$Algo$Math$Node(
								{
									af: A2(
										$elm$core$List$cons,
										$author$project$Algo$Math$Text('('),
										_Utils_ap(
											tokens,
											_List_fromArray(
												[
													$author$project$Algo$Math$Text(')')
												]))),
									b: $author$project$Algo$Math$getState(root)
								}) : $author$project$Algo$Math$Node(
								{
									af: tokens,
									b: $author$project$Algo$Math$getState(root)
								});
						}
				}
			}
			var _v4 = _v3.a;
			return $author$project$Algo$Math$Node(
				{
					af: tokens,
					b: $author$project$Algo$Math$getState(root)
				});
		}(
			function () {
				switch (root.$) {
					case 0:
						var s = root.a;
						return _List_fromArray(
							[
								$author$project$Algo$Math$Text(
								$elm$core$String$fromFloat(s.x))
							]);
					case 1:
						var s = root.a;
						return ($elm$core$String$length(s.a) === 1) ? _List_fromArray(
							[
								$author$project$Algo$Math$Text(s.a)
							]) : _List_fromArray(
							[
								$author$project$Algo$Math$Text('\\' + s.a)
							]);
					case 2:
						var s = root.a;
						return (s.a === '/') ? (multiplicativeFirst ? _List_fromArray(
							[
								$author$project$Algo$Math$Text('1/'),
								A3(
								$author$project$Algo$Math$symbolicateRecursive_,
								$elm$core$Maybe$Just(root),
								false,
								s.N)
							]) : _List_fromArray(
							[
								$author$project$Algo$Math$Text('/'),
								A3(
								$author$project$Algo$Math$symbolicateRecursive_,
								$elm$core$Maybe$Just(root),
								false,
								s.N)
							])) : _List_fromArray(
							[
								$author$project$Algo$Math$Text(s.a),
								A3(
								$author$project$Algo$Math$symbolicateRecursive_,
								$elm$core$Maybe$Just(root),
								true,
								s.N)
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
												$author$project$Algo$Math$symbolicateRecursive_,
												$elm$core$Maybe$Just(root),
												true,
												child)
											]);
									} else {
										if (child.$ === 2) {
											var c = child.a;
											var _v2 = _Utils_Tuple2(s.a, c.a);
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
																		$author$project$Algo$Math$symbolicateRecursive_,
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
																		$author$project$Algo$Math$symbolicateRecursive_,
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
														$author$project$Algo$Math$Text(s.a),
														A3(
														$author$project$Algo$Math$symbolicateRecursive_,
														$elm$core$Maybe$Just(root),
														true,
														child)
													]));
										} else {
											return _Utils_ap(
												result,
												_List_fromArray(
													[
														$author$project$Algo$Math$Text(s.a),
														A3(
														$author$project$Algo$Math$symbolicateRecursive_,
														$elm$core$Maybe$Just(root),
														true,
														child)
													]));
										}
									}
								}),
							_List_Nil,
							s.af);
					case 4:
						var s = root.a;
						return function (_arguments) {
							return A2(
								$elm$core$List$cons,
								$author$project$Algo$Math$Text('\\' + (s.a + '(')),
								_Utils_ap(
									_arguments,
									_List_fromArray(
										[
											$author$project$Algo$Math$Text(')')
										])));
						}(
							A2(
								$elm$core$List$intersperse,
								$author$project$Algo$Math$Text(','),
								A2(
									$elm$core$List$map,
									A2(
										$author$project$Algo$Math$symbolicateRecursive_,
										$elm$core$Maybe$Just(root),
										true),
									s.af)));
					default:
						var s = root.a;
						return A2(
							$elm$core$List$intersperse,
							$author$project$Algo$Math$Text(s.a),
							A2(
								$elm$core$List$map,
								A2(
									$author$project$Algo$Math$symbolicateRecursive_,
									$elm$core$Maybe$Just(root),
									true),
								s.af));
				}
			}());
	});
var $author$project$Algo$Math$symbolicate = function (root) {
	return A3($author$project$Algo$Math$symbolicateRecursive_, $elm$core$Maybe$Nothing, true, root);
};
var $author$project$Algo$Math$toString = function (root) {
	return $author$project$Algo$Math$eqToString_(
		$author$project$Algo$Math$symbolicate(root));
};
var $author$project$Components$Query$setEquations = F2(
	function (dict, model) {
		return function (result) {
			return _Utils_update(
				model,
				{f: result});
		}(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var elem = _v0.b;
					return $author$project$Algo$Math$toString(elem.ar);
				},
				$elm$core$Dict$toList(dict)));
	});
var $author$project$Main$updateQuery_ = F2(
	function (model, dModel) {
		var query = A2(
			$author$project$Components$Query$setEquations,
			$author$project$Components$Display$listEquations(dModel),
			model.bR);
		return $author$project$Components$Query$pushUrl(query);
	});
var $author$project$Main$applyChange_ = F2(
	function (params, model) {
		var swappable = model.H;
		var _v0 = A3($author$project$Components$Display$transformEquation, params.cs, params.a3, swappable.i);
		if (_v0.$ === 1) {
			var errStr = _v0.a;
			return A2($author$project$Main$submitNotification_, model, errStr);
		} else {
			var newDisplay = _v0.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						t: $elm$core$Maybe$Nothing,
						H: _Utils_update(
							swappable,
							{i: newDisplay})
					}),
				A2($author$project$Main$updateQuery_, model, newDisplay));
		}
	});
var $author$project$UI$Animation$delete = function (element) {
	return element.ce ? _Utils_Tuple2(element, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
		_Utils_update(
			element,
			{ce: true}),
		element.a0(0));
};
var $author$project$Components$Rules$toJavascriptString_ = F3(
	function (model, name, children) {
		var _v0 = A2($elm$core$Dict$get, name, model.y);
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('Unable to evaluate the unknown function');
		} else {
			var _v1 = _v0.a;
			var f = _v1.a;
			var _v2 = f.ak;
			switch (_v2.$) {
				case 0:
					var jsName = _v2.a;
					return $elm$core$Result$Ok(
						A2($elm$core$String$join, jsName, children));
				case 1:
					var jsName = _v2.a;
					return ($elm$core$List$length(children) !== 1) ? $elm$core$Result$Err('Prefix can only be for unary operators') : $elm$core$Result$Ok(
						_Utils_ap(
							jsName,
							A2($elm$core$String$join, '', children)));
				default:
					var jsName = _v2.a;
					return $elm$core$Result$Ok(
						jsName + ('(' + (A2($elm$core$String$join, ',', children) + ')')));
			}
		}
	});
var $author$project$Components$Rules$evaluateStr = F2(
	function (model, root) {
		return A2(
			$elm$core$Result$map,
			function (str) {
				return '(' + (str + ')');
			},
			function () {
				switch (root.$) {
					case 0:
						var s = root.a;
						return $elm$core$Result$Ok(
							$elm$core$String$fromFloat(s.x));
					case 1:
						var s = root.a;
						var _v1 = A2($elm$core$Dict$get, s.a, model.C);
						if (_v1.$ === 1) {
							return $elm$core$Result$Err('Unable to evaluate an unknown variable');
						} else {
							var _v2 = _v1.a;
							var str = _v2.a;
							return $elm$core$Result$Ok(str);
						}
					case 2:
						var s = root.a;
						return A2(
							$elm$core$Result$andThen,
							function (child) {
								return A3(
									$author$project$Components$Rules$toJavascriptString_,
									model,
									s.a,
									_List_fromArray(
										[child]));
							},
							A2($author$project$Components$Rules$evaluateStr, model, s.N));
					case 3:
						var s = root.a;
						return A2(
							$elm$core$Result$andThen,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$List$reverse,
								A2($author$project$Components$Rules$toJavascriptString_, model, s.a)),
							A3(
								$author$project$Helper$resultList,
								F2(
									function (child, list) {
										return A2(
											$elm$core$Result$map,
											function (c) {
												return A2($elm$core$List$cons, c, list);
											},
											A2($author$project$Components$Rules$evaluateStr, model, child));
									}),
								_List_Nil,
								s.af));
					case 5:
						return $elm$core$Result$Err('Cannot evaluate a declaration');
					default:
						var s = root.a;
						return A2(
							$elm$core$Result$andThen,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$List$reverse,
								A2($author$project$Components$Rules$toJavascriptString_, model, s.a)),
							A3(
								$author$project$Helper$resultList,
								F2(
									function (child, list) {
										return A2(
											$elm$core$Result$map,
											function (c) {
												return A2($elm$core$List$cons, c, list);
											},
											A2($author$project$Components$Rules$evaluateStr, model, child));
									}),
								_List_Nil,
								s.af));
				}
			}());
	});
var $author$project$UI$Dialog$fieldID = $elm$core$Basics$append('dialog_');
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $author$project$Components$Evaluate$finish = F2(
	function (id, model) {
		var _v0 = A2($elm$core$Dict$get, id, model.ab);
		if (_v0.$ === 1) {
			return _Utils_Tuple2(model, $elm$core$Maybe$Nothing);
		} else {
			var c = _v0.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						ab: A2($elm$core$Dict$remove, id, model.ab)
					}),
				$elm$core$Maybe$Just(c));
		}
	});
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (!maybe.$) {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $author$project$Components$Rules$functionProperties = function (model) {
	return function (dict) {
		return A3(
			$elm$core$Dict$foldl,
			F2(
				function (k, _v2) {
					return A2(
						$elm$core$Dict$insert,
						k,
						{e: 0, j: false, g: false});
				}),
			dict,
			model.C);
	}(
		A2(
			$elm$core$Dict$map,
			F2(
				function (_v0, _v1) {
					var f = _v1.a;
					return f.k;
				}),
			model.y));
};
var $author$project$Algo$Matcher$groupPartition_ = function (check) {
	return A2(
		$elm$core$List$foldl,
		F2(
			function (elem, _v0) {
				var pre = _v0.a;
				var group = _v0.b;
				var post = _v0.c;
				return check(elem) ? _Utils_Tuple3(
					pre,
					A2($elm$core$List$cons, elem, group),
					post) : ($elm$core$List$isEmpty(group) ? _Utils_Tuple3(
					A2($elm$core$List$cons, elem, pre),
					group,
					post) : _Utils_Tuple3(
					pre,
					group,
					A2($elm$core$List$cons, elem, post)));
			}),
		_Utils_Tuple3(_List_Nil, _List_Nil, _List_Nil));
};
var $author$project$Algo$Matcher$groupSubtree = F3(
	function (id, nodes, eq) {
		return A3(
			$author$project$Algo$Matcher$processSubtree_,
			A2($author$project$Algo$Matcher$searchPath_, eq.m.aJ, id),
			function (subEq) {
				var _v0 = subEq.ar;
				if (_v0.$ === 3) {
					var n = _v0.a;
					if (!n.j) {
						return $elm$core$Result$Err('Node is not associative');
					} else {
						if (!n.g) {
							return $elm$core$Result$Err('Not implemented for non-commutative');
						} else {
							var _v1 = A2(
								$author$project$Algo$Matcher$groupPartition_,
								function (c) {
									return A2(
										$elm$core$Set$member,
										$author$project$Algo$Matcher$getID(
											$author$project$Algo$Math$getState(c)),
										nodes);
								},
								n.af);
							var pre = _v1.a;
							var group = _v1.b;
							var post = _v1.c;
							if ((!($elm$core$List$length(pre) + $elm$core$List$length(post))) || $elm$core$List$isEmpty(group)) {
								return $elm$core$Result$Err('Grouping all or none does nothing');
							} else {
								var _v2 = A3(
									$author$project$Algo$Matcher$addNode_,
									$elm$core$Maybe$Nothing,
									$author$project$Algo$Matcher$getID(n.b),
									subEq.m);
								var newS = _v2.a;
								var newT = _v2.b;
								var newP = A3(
									$elm$core$List$foldl,
									function (c) {
										return A2(
											$elm$core$Dict$insert,
											$author$project$Algo$Matcher$getID(
												$author$project$Algo$Math$getState(c)),
											$author$project$Algo$Matcher$getID(newS));
									},
									newT.aJ,
									group);
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										$author$project$Algo$Matcher$getID(newS),
										{
											ar: $author$project$Algo$Math$BinaryNode(
												_Utils_update(
													n,
													{
														af: _Utils_ap(
															$elm$core$List$reverse(pre),
															A2(
																$elm$core$List$cons,
																$author$project$Algo$Math$BinaryNode(
																	_Utils_update(
																		n,
																		{
																			af: $elm$core$List$reverse(group),
																			b: newS
																		})),
																$elm$core$List$reverse(post)))
													})),
											m: _Utils_update(
												newT,
												{aJ: newP})
										}));
							}
						}
					}
				} else {
					return $elm$core$Result$Err('Node is not associative');
				}
			},
			eq);
	});
var $author$project$Components$Display$groupChildren = F4(
	function (eqNum, root, children, model) {
		var _v0 = A2($elm$core$Dict$get, eqNum, model.f);
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('Equation not found');
		} else {
			var eq = _v0.a;
			return A2(
				$elm$core$Result$map,
				function (_v1) {
					var newSelect = _v1.a;
					var newEq = _v1.b;
					return _Utils_update(
						model,
						{
							f: A3(
								$elm$core$Dict$insert,
								eqNum,
								A2($author$project$Algo$History$add, newEq, eq),
								model.f),
							n: $elm$core$Maybe$Just(
								_Utils_Tuple2(
									eqNum,
									$elm$core$Set$singleton(newSelect)))
						});
				},
				A3(
					$author$project$Algo$Matcher$groupSubtree,
					root,
					children,
					$author$project$Algo$History$current(eq)));
		}
	});
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
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === -2) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Helper$listIndex = F2(
	function (num, list) {
		listIndex:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var other = list.b;
				if (!num) {
					return $elm$core$Maybe$Just(x);
				} else {
					var $temp$num = num - 1,
						$temp$list = other;
					num = $temp$num;
					list = $temp$list;
					continue listIndex;
				}
			}
		}
	});
var $author$project$Main$CloseDialog = {$: 18};
var $author$project$Main$ConvertSubString = F4(
	function (a, b, c, d) {
		return {$: 28, a: a, b: b, c: c, d: d};
	});
var $author$project$UI$Dialog$Text = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$numSubDialog_ = F3(
	function (eqNum, root, target) {
		return {
			aQ: $author$project$Main$CloseDialog,
			aG: $elm$core$Maybe$Just('expr'),
			aW: _List_fromArray(
				[
					{
					aw: _List_fromArray(
						[
							_List_fromArray(
							[
								$author$project$UI$Dialog$Text(
								{ci: 'expr'})
							])
						]),
					aB: 'The expression to replace ' + $elm$core$String$fromFloat(target)
				}
				]),
			a_: function (dict) {
				var _v0 = A2($elm$core$Dict$get, 'expr', dict);
				if ((!_v0.$) && (!_v0.a.$)) {
					var val = _v0.a.a;
					return A4($author$project$Main$ConvertSubString, eqNum, root, target, val);
				} else {
					return $author$project$Main$NoOp;
				}
			},
			a$: 'Substitute a number for an expression'
		};
	});
var $author$project$Main$ApplyParameters = function (a) {
	return {$: 26, a: a};
};
var $author$project$UI$Dialog$Function = function (a) {
	return {$: 4, a: a};
};
var $author$project$UI$Dialog$Info = function (a) {
	return {$: 2, a: a};
};
var $author$project$UI$Dialog$Radio = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$parameterDialog_ = function (params) {
	return {
		aQ: $author$project$Main$CloseDialog,
		aG: $elm$core$Maybe$Nothing,
		aW: function (sections) {
			return ($elm$core$List$length(params.aT) <= 1) ? sections : A2(
				$elm$core$List$cons,
				{
					aw: _List_fromArray(
						[
							_List_fromArray(
							[
								$author$project$UI$Dialog$Info(
								{aL: 'Select the pattern'})
							]),
							_List_fromArray(
							[
								$author$project$UI$Dialog$Radio(
								{
									a: '_method',
									bH: $elm$core$Dict$fromList(
										A2(
											$elm$core$List$indexedMap,
											F2(
												function (k, m) {
													return _Utils_Tuple2(k, m.a);
												}),
											params.aT))
								})
							])
						]),
					aB: ''
				},
				sections);
		}(
			_List_fromArray(
				[
					{
					aw: A2(
						$elm$core$List$map,
						function (_v0) {
							var key = _v0.a;
							var param = _v0.b;
							return (!param.e) ? _List_fromArray(
								[
									$author$project$UI$Dialog$Info(
									{aL: param.a + '= '}),
									$author$project$UI$Dialog$Text(
									{ci: param.a}),
									$author$project$UI$Dialog$Info(
									{aL: param.ag})
								]) : _List_fromArray(
								[
									$author$project$UI$Dialog$Function(
									{e: param.e, a: key}),
									$author$project$UI$Dialog$Info(
									{aL: param.ag})
								]);
						},
						$elm$core$Dict$toList(params.bb)),
					aB: 'Fill in the parameters'
				}
				])),
		a_: $author$project$Main$ApplyParameters,
		a$: 'Set parameters for ' + params.a$
	};
};
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$Set$filter = F2(
	function (isGood, _v0) {
		var dict = _v0;
		return A2(
			$elm$core$Dict$filter,
			F2(
				function (key, _v1) {
					return isGood(key);
				}),
			dict);
	});
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$isEmpty(dict);
};
var $author$project$Components$Display$newSelectedNodes_ = F2(
	function (selected, eq) {
		var intersection = A2(
			$elm$core$Set$filter,
			function (n) {
				return A2($elm$core$Dict$member, n, eq.m.aJ);
			},
			selected);
		return $elm$core$Set$isEmpty(intersection) ? $elm$core$Set$singleton(
			$author$project$Algo$Matcher$getID(
				$author$project$Algo$Math$getState(eq.ar))) : intersection;
	});
var $author$project$Algo$History$redo = function (model) {
	var _v0 = model.ad;
	if (!_v0.b) {
		return model;
	} else {
		var x = _v0.a;
		var others = _v0.b;
		return _Utils_update(
			model,
			{
				ad: others,
				I: A2($elm$core$List$cons, x, model.I)
			});
	}
};
var $author$project$Components$Display$redo = function (model) {
	var _v0 = model.n;
	if (_v0.$ === 1) {
		return $elm$core$Result$Err('No equation selected to redo');
	} else {
		var _v1 = _v0.a;
		var eqNum = _v1.a;
		var selected = _v1.b;
		var _v2 = A2($elm$core$Dict$get, eqNum, model.f);
		if (_v2.$ === 1) {
			return $elm$core$Result$Err('Equation not found to redo');
		} else {
			var his = _v2.a;
			var newHis = $author$project$Algo$History$redo(his);
			return $elm$core$Result$Ok(
				_Utils_update(
					model,
					{
						f: A3($elm$core$Dict$insert, eqNum, newHis, model.f),
						n: $elm$core$Maybe$Just(
							_Utils_Tuple2(
								eqNum,
								A2(
									$author$project$Components$Display$newSelectedNodes_,
									selected,
									$author$project$Algo$History$current(newHis))))
					}));
		}
	}
};
var $author$project$Components$Display$replaceNodeWithNumber = F4(
	function (eqNum, id, number, model) {
		var _v0 = A2($elm$core$Dict$get, eqNum, model.f);
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('Equation not found');
		} else {
			var eq = _v0.a;
			var replacement = (number < 0) ? $author$project$Algo$Math$UnaryNode(
				{
					N: $author$project$Algo$Math$RealNode(
						{b: $elm$core$Maybe$Nothing, x: -number}),
					a: '-',
					b: $elm$core$Maybe$Nothing
				}) : $author$project$Algo$Math$RealNode(
				{b: $elm$core$Maybe$Nothing, x: number});
			return A2(
				$elm$core$Result$map,
				function (_v1) {
					var newSelect = _v1.a;
					var newEq = _v1.b;
					return _Utils_update(
						model,
						{
							f: A3(
								$elm$core$Dict$insert,
								eqNum,
								A2($author$project$Algo$History$add, newEq, eq),
								model.f),
							n: $elm$core$Maybe$Just(
								_Utils_Tuple2(
									eqNum,
									$elm$core$Set$singleton(newSelect)))
						});
				},
				A4(
					$author$project$Algo$Matcher$replaceSubtree,
					$elm$core$Set$singleton(id),
					replacement,
					$elm$core$Dict$empty,
					$author$project$Algo$History$current(eq)));
		}
	});
var $author$project$Algo$Matcher$replaceRealNode = F4(
	function (id, target, subtree, eq) {
		return A3(
			$author$project$Algo$Matcher$processSubtree_,
			A2($author$project$Algo$Matcher$searchPath_, eq.m.aJ, id),
			function (subEq) {
				var _v0 = subEq.ar;
				if (!_v0.$) {
					var n = _v0.a;
					return (!_Utils_eq(target, n.x)) ? $elm$core$Result$Err('Expression does not equal to the node\'s value') : function (_v1) {
						var root = _v1.a;
						var tracker = _v1.b;
						var parent = A2($elm$core$Dict$get, id, tracker.aJ);
						var nextTracker = _Utils_update(
							tracker,
							{
								aJ: A2(
									$elm$core$Dict$remove,
									$author$project$Algo$Matcher$getID(n.b),
									tracker.aJ)
							});
						return $elm$core$Result$Ok(
							_Utils_Tuple2(
								$author$project$Algo$Matcher$getID(
									$author$project$Algo$Math$getState(root)),
								{
									ar: root,
									m: A3($author$project$Algo$Matcher$setParent_, root, parent, nextTracker)
								}));
					}(
						A2($author$project$Algo$Matcher$processID_, subEq.m, subtree));
				} else {
					return $elm$core$Result$Err('Node is not a number');
				}
			},
			eq);
	});
var $author$project$Components$Display$replaceNumber = F5(
	function (eqNum, root, target, replacement, model) {
		var _v0 = A2($elm$core$Dict$get, eqNum, model.f);
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('Equation not found');
		} else {
			var eq = _v0.a;
			return A2(
				$elm$core$Result$map,
				function (_v1) {
					var newSelect = _v1.a;
					var newEq = _v1.b;
					return _Utils_update(
						model,
						{
							f: A3(
								$elm$core$Dict$insert,
								eqNum,
								A2($author$project$Algo$History$add, newEq, eq),
								model.f),
							n: $elm$core$Maybe$Just(
								_Utils_Tuple2(
									eqNum,
									$elm$core$Set$singleton(newSelect)))
						});
				},
				A4(
					$author$project$Algo$Matcher$replaceRealNode,
					root,
					target,
					replacement,
					$author$project$Algo$History$current(eq)));
		}
	});
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(0),
				dictionary));
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $author$project$Algo$History$encodeNode_ = F2(
	function (convert, node) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'parent',
					$elm$json$Json$Encode$int(node.aJ)),
					_Utils_Tuple2(
					'component',
					convert(node.at)),
					_Utils_Tuple2(
					'children',
					A2(
						$elm$json$Json$Encode$list,
						function (c) {
							return $elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'index',
										$elm$json$Json$Encode$int(c._)),
										_Utils_Tuple2(
										'height',
										$elm$json$Json$Encode$int(c.F)),
										_Utils_Tuple2(
										'width',
										$elm$json$Json$Encode$int(c.J))
									]));
						},
						node.af))
				]));
	});
var $author$project$Algo$History$encode = F2(
	function (convert, model) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'root',
					A2($author$project$Algo$History$encodeNode_, convert, model.ar)),
					_Utils_Tuple2(
					'nodes',
					A3(
						$elm$json$Json$Encode$dict,
						$elm$core$String$fromInt,
						$author$project$Algo$History$encodeNode_(convert),
						model.R)),
					_Utils_Tuple2(
					'visits',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$int, model.I)),
					_Utils_Tuple2(
					'undone',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$int, model.ad))
				]));
	});
var $elm$json$Json$Encode$float = _Json_wrap;
var $author$project$Algo$Math$encode = F2(
	function (converter, root) {
		switch (root.$) {
			case 0:
				var s = root.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'state',
							converter(s.b)),
							_Utils_Tuple2(
							'value',
							$elm$json$Json$Encode$float(s.x)),
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('real'))
						]));
			case 1:
				var s = root.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'state',
							converter(s.b)),
							_Utils_Tuple2(
							'name',
							$elm$json$Json$Encode$string(s.a)),
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('variable'))
						]));
			case 2:
				var s = root.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'state',
							converter(s.b)),
							_Utils_Tuple2(
							'name',
							$elm$json$Json$Encode$string(s.a)),
							_Utils_Tuple2(
							'child',
							A2($author$project$Algo$Math$encode, converter, s.N)),
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('unary'))
						]));
			case 4:
				var s = root.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'state',
							converter(s.b)),
							_Utils_Tuple2(
							'name',
							$elm$json$Json$Encode$string(s.a)),
							_Utils_Tuple2(
							'children',
							A2(
								$elm$json$Json$Encode$list,
								$author$project$Algo$Math$encode(converter),
								s.af)),
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('generic'))
						]));
			case 5:
				var s = root.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'state',
							converter(s.b)),
							_Utils_Tuple2(
							'name',
							$elm$json$Json$Encode$string(s.a)),
							_Utils_Tuple2(
							'children',
							A2(
								$elm$json$Json$Encode$list,
								$author$project$Algo$Math$encode(converter),
								s.af)),
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('declarative'))
						]));
			default:
				var s = root.a;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'state',
							converter(s.b)),
							_Utils_Tuple2(
							'name',
							$elm$json$Json$Encode$string(s.a)),
							_Utils_Tuple2(
							'associative',
							$elm$json$Json$Encode$bool(s.j)),
							_Utils_Tuple2(
							'commutative',
							$elm$json$Json$Encode$bool(s.g)),
							_Utils_Tuple2(
							'children',
							A2(
								$elm$json$Json$Encode$list,
								$author$project$Algo$Math$encode(converter),
								s.af)),
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('binary'))
						]));
		}
	});
var $author$project$Algo$Matcher$encodeState_ = F2(
	function (convert, s) {
		var id = s.a;
		var innerState = s.b;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$int(id)),
					_Utils_Tuple2(
					'state',
					convert(innerState))
				]));
	});
var $author$project$Algo$Matcher$encodeEquation = F2(
	function (convert, eq) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'root',
					A2(
						$author$project$Algo$Math$encode,
						$author$project$Algo$Matcher$encodeState_(convert),
						eq.ar)),
					_Utils_Tuple2(
					'tracker',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'nextID',
								$elm$json$Json$Encode$int(eq.m.o)),
								_Utils_Tuple2(
								'parent',
								A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, $elm$json$Json$Encode$int, eq.m.aJ))
							])))
				]));
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$set = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$Set$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $author$project$Components$Display$encode = function (model) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'equations',
				A3(
					$elm$json$Json$Encode$dict,
					$elm$core$String$fromInt,
					$author$project$Algo$History$encode(
						$author$project$Algo$Matcher$encodeEquation(
							function (s) {
								return $elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'prevID',
											$elm$json$Json$Encode$int(s.ap))
										]));
							})),
					model.f)),
				_Utils_Tuple2(
				'nextEquationNum',
				$elm$json$Json$Encode$int(model.an)),
				_Utils_Tuple2(
				'hidden',
				A2(
					$elm$json$Json$Encode$list,
					$elm$json$Json$Encode$int,
					$elm$core$Set$toList(model.P))),
				_Utils_Tuple2(
				'selected',
				function () {
					var _v0 = model.n;
					if (_v0.$ === 1) {
						return $elm$json$Json$Encode$null;
					} else {
						var _v1 = _v0.a;
						var eq = _v1.a;
						var nodes = _v1.b;
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'eq',
									$elm$json$Json$Encode$int(eq)),
									_Utils_Tuple2(
									'nodes',
									A2($elm$json$Json$Encode$set, $elm$json$Json$Encode$int, nodes))
								]));
					}
				}()),
				_Utils_Tuple2(
				'createModeForEquation',
				function () {
					var _v2 = model.aR;
					if (_v2.$ === 1) {
						return $elm$json$Json$Encode$null;
					} else {
						var n = _v2.a;
						return $elm$json$Json$Encode$int(n);
					}
				}())
			]));
};
var $author$project$Components$Evaluate$encode = F2(
	function (converter, model) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nextCallID',
					$elm$json$Json$Encode$int(model.al)),
					_Utils_Tuple2(
					'ongoing',
					A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, converter, model.ab))
				]));
	});
var $author$project$Components$Rules$encodeFProp_ = function (prop) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'arguments',
				$elm$json$Json$Encode$int(prop.k.e)),
				_Utils_Tuple2(
				'associative',
				$elm$json$Json$Encode$bool(prop.k.j)),
				_Utils_Tuple2(
				'commutative',
				$elm$json$Json$Encode$bool(prop.k.g)),
				_Utils_Tuple2(
				'javascript',
				function () {
					var _v0 = prop.ak;
					switch (_v0.$) {
						case 0:
							var js = _v0.a;
							return $elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'type',
										$elm$json$Json$Encode$string('infix')),
										_Utils_Tuple2(
										'symbol',
										$elm$json$Json$Encode$string(js))
									]));
						case 1:
							var js = _v0.a;
							return $elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'type',
										$elm$json$Json$Encode$string('prefix')),
										_Utils_Tuple2(
										'symbol',
										$elm$json$Json$Encode$string(js))
									]));
						default:
							var js = _v0.a;
							return $elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'type',
										$elm$json$Json$Encode$string('function')),
										_Utils_Tuple2(
										'symbol',
										$elm$json$Json$Encode$string(js))
									]));
					}
				}())
			]));
};
var $author$project$Components$Rules$encodeParameter_ = function (param) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'usage',
				$elm$json$Json$Encode$string(param.a)),
				_Utils_Tuple2(
				'arguments',
				$elm$json$Json$Encode$int(param.e)),
				_Utils_Tuple2(
				'description',
				$elm$json$Json$Encode$string(param.ag))
			]));
};
var $author$project$Components$Rules$encodeRule_ = function (rule) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$string(rule.a$)),
				_Utils_Tuple2(
				'description',
				$elm$json$Json$Encode$string(rule.ag)),
				_Utils_Tuple2(
				'parameters',
				A3($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$Components$Rules$encodeParameter_, rule.bb)),
				_Utils_Tuple2(
				'matches',
				A2(
					$elm$json$Json$Encode$list,
					function (match) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'from',
									$elm$json$Json$Encode$string(match.a3.a)),
									_Utils_Tuple2(
									'to',
									$elm$json$Json$Encode$string(match.bj.a))
								]));
					},
					rule.aT))
			]));
};
var $author$project$Components$Rules$encodeTopic_ = function (topic) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(topic.a)),
				_Utils_Tuple2(
				'constants',
				A3($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $elm$json$Json$Encode$string, topic.C)),
				_Utils_Tuple2(
				'functions',
				A3($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$Components$Rules$encodeFProp_, topic.y)),
				_Utils_Tuple2(
				'actions',
				A2($elm$json$Json$Encode$list, $author$project$Components$Rules$encodeRule_, topic.bd))
			]));
};
var $author$project$Helper$maybeAppend = F2(
	function (thing, list) {
		if (thing.$ === 1) {
			return list;
		} else {
			var result = thing.a;
			return _Utils_ap(
				list,
				_List_fromArray(
					[result]));
		}
	});
var $author$project$Components$Rules$encode = function (model) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'functions',
				A3(
					$elm$json$Json$Encode$dict,
					$elm$core$Basics$identity,
					function (_v0) {
						var prop = _v0.a;
						var count = _v0.b;
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'properties',
									$author$project$Components$Rules$encodeFProp_(prop)),
									_Utils_Tuple2(
									'count',
									$elm$json$Json$Encode$int(count))
								]));
					},
					model.y)),
				_Utils_Tuple2(
				'constants',
				A3(
					$elm$json$Json$Encode$dict,
					$elm$core$Basics$identity,
					function (_v1) {
						var name = _v1.a;
						var count = _v1.b;
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'name',
									$elm$json$Json$Encode$string(name)),
									_Utils_Tuple2(
									'count',
									$elm$json$Json$Encode$int(count))
								]));
					},
					model.C)),
				_Utils_Tuple2(
				'topics',
				A3(
					$elm$json$Json$Encode$dict,
					$elm$core$Basics$identity,
					function (loadState) {
						if (!loadState.$) {
							var url = loadState.a;
							return $elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'type',
										$elm$json$Json$Encode$string('notInstalled')),
										_Utils_Tuple2(
										'url',
										$elm$json$Json$Encode$string(url))
									]));
						} else {
							var url = loadState.a;
							var topic = loadState.b;
							return $elm$json$Json$Encode$object(
								A2(
									$author$project$Helper$maybeAppend,
									A2(
										$elm$core$Maybe$map,
										function (str) {
											return _Utils_Tuple2(
												'url',
												$elm$json$Json$Encode$string(str));
										},
										url),
									_List_fromArray(
										[
											_Utils_Tuple2(
											'type',
											$elm$json$Json$Encode$string('installed')),
											_Utils_Tuple2(
											'topic',
											$author$project$Components$Rules$encodeTopic_(topic))
										])));
						}
					},
					model.v))
			]));
};
var $author$project$Components$Tutorial$encode = function (_v0) {
	return $elm$json$Json$Encode$null;
};
var $author$project$UI$Animation$encode = F2(
	function (innerEnc, element) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'element',
					innerEnc(element.br)),
					_Utils_Tuple2(
					'deleting',
					$elm$json$Json$Encode$bool(element.ce))
				]));
	});
var $author$project$UI$Draggable$encode = function (model) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'left',
				$elm$json$Json$Encode$float(model.q.w)),
				_Utils_Tuple2(
				'right',
				$elm$json$Json$Encode$float(model.q.A)),
				_Utils_Tuple2(
				'top',
				$elm$json$Json$Encode$float(model.q.E)),
				_Utils_Tuple2(
				'bottom',
				$elm$json$Json$Encode$float(model.q.B)),
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$string(model.ci))
			]));
};
var $author$project$UI$Menu$encode = function (model) {
	return A2($elm$json$Json$Encode$set, $elm$json$Json$Encode$string, model.V);
};
var $author$project$UI$Notification$encode = function (model) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'nextID',
				$elm$json$Json$Encode$int(model.o)),
				_Utils_Tuple2(
				'notifications',
				A3(
					$elm$json$Json$Encode$dict,
					$elm$core$String$fromInt,
					$author$project$UI$Animation$encode($elm$json$Json$Encode$string),
					model.M))
			]));
};
var $author$project$Algo$Matcher$encodeReplacement = $author$project$Algo$Math$encode(
	function (arg) {
		if (arg.$ === 1) {
			return $elm$json$Json$Encode$null;
		} else {
			var n = arg.a;
			return $elm$json$Json$Encode$int(n);
		}
	});
var $elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var $author$project$Main$saveFile = function (model) {
	return A3(
		$elm$file$File$Download$string,
		'math.json',
		'application/json',
		A2(
			$elm$json$Json$Encode$encode,
			0,
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'display',
						$author$project$Components$Display$encode(model.i)),
						_Utils_Tuple2(
						'rules',
						$author$project$Components$Rules$encode(model.bd)),
						_Utils_Tuple2(
						'tutorial',
						$author$project$Components$Tutorial$encode(model.aC)),
						_Utils_Tuple2(
						'notification',
						$author$project$UI$Notification$encode(model.aa)),
						_Utils_Tuple2(
						'menu',
						$author$project$UI$Menu$encode(model.ax)),
						_Utils_Tuple2(
						'evaluator',
						A2(
							$author$project$Components$Evaluate$encode,
							function (t) {
								if (!t.$) {
									var eq = t.a;
									var node = t.b;
									var f = t.c;
									var replacement = t.d;
									return $elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'eq',
												$elm$json$Json$Encode$int(eq)),
												_Utils_Tuple2(
												'node',
												$elm$json$Json$Encode$int(node)),
												_Utils_Tuple2(
												'target',
												$elm$json$Json$Encode$float(f)),
												_Utils_Tuple2(
												'replacement',
												$author$project$Algo$Matcher$encodeReplacement(replacement)),
												_Utils_Tuple2(
												'type',
												$elm$json$Json$Encode$string('numSub'))
											]));
								} else {
									var eq = t.a;
									var node = t.b;
									return $elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'eq',
												$elm$json$Json$Encode$int(eq)),
												_Utils_Tuple2(
												'node',
												$elm$json$Json$Encode$int(node)),
												_Utils_Tuple2(
												'type',
												$elm$json$Json$Encode$string('eval'))
											]));
								}
							},
							model.O)),
						_Utils_Tuple2(
						'createMode',
						function () {
							var _v1 = model.D;
							if (_v1.$ === 1) {
								return $elm$json$Json$Encode$null;
							} else {
								var m = _v1.a;
								return A2($author$project$UI$Animation$encode, $elm$json$Json$Encode$int, m);
							}
						}()),
						_Utils_Tuple2(
						'nextCreateInt',
						$elm$json$Json$Encode$int(model.am)),
						_Utils_Tuple2(
						'showHelp',
						$elm$json$Json$Encode$bool(model.U)),
						_Utils_Tuple2(
						'showMenu',
						$elm$json$Json$Encode$bool(model.Y)),
						_Utils_Tuple2(
						'showHistory',
						$elm$json$Json$Encode$bool(model.az)),
						_Utils_Tuple2(
						'historyBox',
						$author$project$UI$Draggable$encode(model.av))
					]))));
};
var $author$project$Components$Evaluate$send = F3(
	function (c, str, model) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					al: model.al + 1,
					ab: A3($elm$core$Dict$insert, model.al, c, model.ab)
				}),
			model.be(
				{ci: model.al, cx: str}));
	});
var $author$project$Main$ApplySubstitution = F3(
	function (a, b, c) {
		return {$: 27, a: a, b: b, c: c};
	});
var $author$project$Main$substitutionDialog_ = F3(
	function (eqNum, root, model) {
		return {
			aQ: $author$project$Main$CloseDialog,
			aG: $elm$core$Maybe$Just('eqNum'),
			aW: _List_fromArray(
				[
					{
					aw: _List_fromArray(
						[
							_List_fromArray(
							[
								$author$project$UI$Dialog$Radio(
								{
									a: 'eqNum',
									bH: A2(
										$elm$core$Dict$map,
										function (_v1) {
											return A2(
												$elm$core$Basics$composeR,
												$author$project$Algo$History$current,
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.ar;
													},
													$author$project$Algo$Math$toString));
										},
										A2(
											$elm$core$Dict$filter,
											F2(
												function (k, _v0) {
													return !_Utils_eq(k, eqNum);
												}),
											model.i.f))
								})
							])
						]),
					aB: 'Select the equation to use for substitution'
				}
				]),
			a_: function (dict) {
				var _v2 = A2($elm$core$Dict$get, 'eqNum', dict);
				if ((!_v2.$) && (_v2.a.$ === 1)) {
					var a = _v2.a.a;
					return A3($author$project$Main$ApplySubstitution, eqNum, root, a);
				} else {
					return $author$project$Main$NoOp;
				}
			},
			a$: 'Substitute a variable for a formula'
		};
	});
var $author$project$Main$Swappable = function (display) {
	return function (rules) {
		return function (tutorial) {
			return function (notification) {
				return function (menu) {
					return function (evaluator) {
						return function (createMode) {
							return function (nextCreateInt) {
								return function (showHelp) {
									return function (showMenu) {
										return function (showHistory) {
											return function (historyBox) {
												return {D: createMode, i: display, O: evaluator, av: historyBox, ax: menu, am: nextCreateInt, aa: notification, bd: rules, U: showHelp, az: showHistory, Y: showMenu, aC: tutorial};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$UI$Animation$decoder = F2(
	function (innerDec, trigger) {
		return A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (e, d) {
					return {ce: d, br: e, a0: trigger};
				}),
			A2($elm$json$Json$Decode$field, 'element', innerDec),
			A2($elm$json$Json$Decode$field, 'deleting', $elm$json$Json$Decode$bool));
	});
var $author$project$Main$createModeDecoder_ = A2(
	$elm$json$Json$Decode$andThen,
	function (id) {
		return A2(
			$author$project$UI$Animation$decoder,
			$elm$json$Json$Decode$int,
			$author$project$Main$uiCancelCmd_(id));
	},
	A2($elm$json$Json$Decode$field, 'element', $elm$json$Json$Decode$int));
var $author$project$Algo$History$Model = F4(
	function (nodes, root, visits, undone) {
		return {R: nodes, ar: root, ad: undone, I: visits};
	});
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Helper$resultToDecoder = function (res) {
	if (res.$ === 1) {
		var str = res.a;
		return $elm$json$Json$Decode$fail(str);
	} else {
		var b = res.a;
		return $elm$json$Json$Decode$succeed(b);
	}
};
var $author$project$Helper$intDictDecoder = function (valDecoder) {
	return A2(
		$elm$json$Json$Decode$andThen,
		A2(
			$elm$core$Basics$composeR,
			A2(
				$author$project$Helper$resultDict,
				F3(
					function (k, v, dict) {
						var _v0 = $elm$core$String$toInt(k);
						if (_v0.$ === 1) {
							return $elm$core$Result$Err('index is not an integer');
						} else {
							var n = _v0.a;
							return $elm$core$Result$Ok(
								A3($elm$core$Dict$insert, n, v, dict));
						}
					}),
				$elm$core$Dict$empty),
			$author$project$Helper$resultToDecoder),
		$elm$json$Json$Decode$dict(valDecoder));
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$Algo$History$Node_ = F3(
	function (parent, component, children) {
		return {af: children, at: component, aJ: parent};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$Algo$History$nodeDecoder_ = function (innerDec) {
	return A4(
		$elm$json$Json$Decode$map3,
		$author$project$Algo$History$Node_,
		A2($elm$json$Json$Decode$field, 'parent', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'component', innerDec),
		A2(
			$elm$json$Json$Decode$field,
			'children',
			$elm$json$Json$Decode$list(
				A4(
					$elm$json$Json$Decode$map3,
					F3(
						function (i, h, w) {
							return {F: h, _: i, J: w};
						}),
					A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$int)))));
};
var $author$project$Algo$History$decoder = function (innerDec) {
	return A5(
		$elm$json$Json$Decode$map4,
		$author$project$Algo$History$Model,
		A2(
			$elm$json$Json$Decode$field,
			'nodes',
			$author$project$Helper$intDictDecoder(
				$author$project$Algo$History$nodeDecoder_(innerDec))),
		A2(
			$elm$json$Json$Decode$field,
			'root',
			$author$project$Algo$History$nodeDecoder_(innerDec)),
		A2(
			$elm$json$Json$Decode$field,
			'visits',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$int)),
		A2(
			$elm$json$Json$Decode$field,
			'undone',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$int)));
};
var $elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		$elm$json$Json$Decode$andThen,
		thunk,
		$elm$json$Json$Decode$succeed(0));
};
var $elm$json$Json$Decode$map5 = _Json_map5;
var $author$project$Algo$Math$decoder = function (stateDecoder) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (t) {
			var sDec = A2($elm$json$Json$Decode$field, 'state', stateDecoder);
			switch (t) {
				case 'real':
					return A3(
						$elm$json$Json$Decode$map2,
						F2(
							function (s, v) {
								return $author$project$Algo$Math$RealNode(
									{b: s, x: v});
							}),
						sDec,
						A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$float));
				case 'variable':
					return A3(
						$elm$json$Json$Decode$map2,
						F2(
							function (s, n) {
								return $author$project$Algo$Math$VariableNode(
									{a: n, b: s});
							}),
						sDec,
						A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string));
				case 'unary':
					return A4(
						$elm$json$Json$Decode$map3,
						F3(
							function (s, n, c) {
								return $author$project$Algo$Math$UnaryNode(
									{N: c, a: n, b: s});
							}),
						sDec,
						A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
						A2(
							$elm$json$Json$Decode$field,
							'child',
							$elm$json$Json$Decode$lazy(
								function (_v1) {
									return $author$project$Algo$Math$decoder(stateDecoder);
								})));
				case 'binary':
					return A6(
						$elm$json$Json$Decode$map5,
						F5(
							function (s, n, a, c, children) {
								return $author$project$Algo$Math$BinaryNode(
									{j: a, af: children, g: c, a: n, b: s});
							}),
						sDec,
						A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
						A2($elm$json$Json$Decode$field, 'associative', $elm$json$Json$Decode$bool),
						A2($elm$json$Json$Decode$field, 'commutative', $elm$json$Json$Decode$bool),
						A2(
							$elm$json$Json$Decode$field,
							'children',
							$elm$json$Json$Decode$list(
								$elm$json$Json$Decode$lazy(
									function (_v2) {
										return $author$project$Algo$Math$decoder(stateDecoder);
									}))));
				case 'generic':
					return A4(
						$elm$json$Json$Decode$map3,
						F3(
							function (s, n, c) {
								return $author$project$Algo$Math$GenericNode(
									{af: c, a: n, b: s});
							}),
						sDec,
						A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
						A2(
							$elm$json$Json$Decode$field,
							'children',
							$elm$json$Json$Decode$list(
								$elm$json$Json$Decode$lazy(
									function (_v3) {
										return $author$project$Algo$Math$decoder(stateDecoder);
									}))));
				case 'declarative':
					return A4(
						$elm$json$Json$Decode$map3,
						F3(
							function (s, n, c) {
								return $author$project$Algo$Math$DeclarativeNode(
									{af: c, a: n, b: s});
							}),
						sDec,
						A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
						A2(
							$elm$json$Json$Decode$field,
							'children',
							$elm$json$Json$Decode$list(
								$elm$json$Json$Decode$lazy(
									function (_v4) {
										return $author$project$Algo$Math$decoder(stateDecoder);
									}))));
				default:
					return $elm$json$Json$Decode$fail('Unexpected type of node: ' + t);
			}
		},
		A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
};
var $author$project$Algo$Matcher$stateDecoder_ = function (innerDec) {
	return A3(
		$elm$json$Json$Decode$map2,
		$author$project$Algo$Matcher$State_,
		A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
		A2($elm$json$Json$Decode$field, 'state', innerDec));
};
var $author$project$Algo$Matcher$equationDecoder = F3(
	function (newState, copyState, innerDec) {
		return A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (root, tracker) {
					return {ar: root, m: tracker};
				}),
			A2(
				$elm$json$Json$Decode$field,
				'root',
				$author$project$Algo$Math$decoder(
					$author$project$Algo$Matcher$stateDecoder_(innerDec))),
			A2(
				$elm$json$Json$Decode$field,
				'tracker',
				A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (id, p) {
							return {aF: copyState, aI: newState, o: id, aJ: p};
						}),
					A2($elm$json$Json$Decode$field, 'nextID', $elm$json$Json$Decode$int),
					A2(
						$elm$json$Json$Decode$field,
						'parent',
						$author$project$Helper$intDictDecoder($elm$json$Json$Decode$int)))));
	});
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Components$Display$decoder = A6(
	$elm$json$Json$Decode$map5,
	F5(
		function (eq, next, hidden, sel, create) {
			return {aR: create, f: eq, P: hidden, an: next, n: sel};
		}),
	A2(
		$elm$json$Json$Decode$field,
		'equations',
		$author$project$Helper$intDictDecoder(
			$author$project$Algo$History$decoder(
				A3(
					$author$project$Algo$Matcher$equationDecoder,
					$author$project$Components$Display$createState,
					$author$project$Components$Display$updateState,
					A2(
						$elm$json$Json$Decode$map,
						function (id) {
							return {ap: id};
						},
						A2($elm$json$Json$Decode$field, 'prevID', $elm$json$Json$Decode$int)))))),
	A2($elm$json$Json$Decode$field, 'nextEquationNum', $elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$field,
		'hidden',
		A2(
			$elm$json$Json$Decode$map,
			$elm$core$Set$fromList,
			$elm$json$Json$Decode$list($elm$json$Json$Decode$int))),
	A2(
		$elm$json$Json$Decode$field,
		'selected',
		$elm$json$Json$Decode$maybe(
			A3(
				$elm$json$Json$Decode$map2,
				$elm$core$Tuple$pair,
				A2($elm$json$Json$Decode$field, 'eq', $elm$json$Json$Decode$int),
				A2(
					$elm$json$Json$Decode$field,
					'nodes',
					A2(
						$elm$json$Json$Decode$map,
						$elm$core$Set$fromList,
						$elm$json$Json$Decode$list($elm$json$Json$Decode$int)))))),
	A2(
		$elm$json$Json$Decode$field,
		'createModeForEquation',
		$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int)));
var $author$project$Components$Evaluate$decoder = F2(
	function (sender, innerDec) {
		return A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (n, o) {
					return {al: n, ab: o, be: sender};
				}),
			A2($elm$json$Json$Decode$field, 'nextCallID', $elm$json$Json$Decode$int),
			A2(
				$elm$json$Json$Decode$field,
				'ongoing',
				$author$project$Helper$intDictDecoder(innerDec)));
	});
var $author$project$Algo$Matcher$FunctionProperties = F3(
	function (_arguments, associative, commutative) {
		return {e: _arguments, j: associative, g: commutative};
	});
var $author$project$Components$Rules$FunctionProperties_ = F2(
	function (properties, javascript) {
		return {ak: javascript, k: properties};
	});
var $author$project$Components$Rules$FuncOp = function (a) {
	return {$: 2, a: a};
};
var $author$project$Components$Rules$javascriptDecoder_ = A2(
	$elm$json$Json$Decode$andThen,
	function (_v0) {
		var t = _v0.a;
		var symbol = _v0.b;
		switch (t) {
			case 'infix':
				return A2(
					$elm$core$String$all,
					function (s) {
						return A2(
							$elm$core$String$contains,
							$elm$core$String$fromChar(s),
							'0123456789+-*/');
					},
					symbol) ? $elm$json$Json$Decode$succeed(
					$author$project$Components$Rules$InfixOp(symbol)) : $elm$json$Json$Decode$fail('Disallowed char in javascript function');
			case 'prefix':
				return A2(
					$elm$core$String$all,
					function (s) {
						return A2(
							$elm$core$String$contains,
							$elm$core$String$fromChar(s),
							'0123456789+-*/');
					},
					symbol) ? $elm$json$Json$Decode$succeed(
					$author$project$Components$Rules$PrefixOp(symbol)) : $elm$json$Json$Decode$fail('Disallowed char in javascript function');
			case 'function':
				return (A2($elm$core$String$left, 5, symbol) !== 'Math.') ? $elm$json$Json$Decode$fail('Only functions from the Math object can be used') : (A2(
					$elm$core$String$all,
					$elm$core$Char$isAlphaNum,
					A2($elm$core$String$dropLeft, 5, symbol)) ? $elm$json$Json$Decode$succeed(
					$author$project$Components$Rules$FuncOp(symbol)) : $elm$json$Json$Decode$fail('Unexpected symbols after \'Math.\''));
			default:
				return $elm$json$Json$Decode$fail('Unknown type of javascript function');
		}
	},
	A3(
		$elm$json$Json$Decode$map2,
		$elm$core$Tuple$pair,
		A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'symbol', $elm$json$Json$Decode$string)));
var $author$project$Components$Rules$functionDecoder_ = A2(
	$elm$json$Json$Decode$andThen,
	function (f) {
		return ((f.k.j || f.k.g) && (f.k.e !== 2)) ? $elm$json$Json$Decode$fail('Associative and Commutative properties only apply to binary functions') : ((f.k.j && (!f.k.g)) ? $elm$json$Json$Decode$fail('Only associative binary nodes is not supported yet') : $elm$json$Json$Decode$succeed(f));
	},
	A5(
		$elm$json$Json$Decode$map4,
		F3(
			function (args, assoc, comm) {
				return $author$project$Components$Rules$FunctionProperties_(
					A3($author$project$Algo$Matcher$FunctionProperties, args, assoc, comm));
			}),
		A2($elm$json$Json$Decode$field, 'arguments', $elm$json$Json$Decode$int),
		$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2($elm$json$Json$Decode$field, 'associative', $elm$json$Json$Decode$bool),
					$elm$json$Json$Decode$succeed(false)
				])),
		$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2($elm$json$Json$Decode$field, 'commutative', $elm$json$Json$Decode$bool),
					$elm$json$Json$Decode$succeed(false)
				])),
		A2($elm$json$Json$Decode$field, 'javascript', $author$project$Components$Rules$javascriptDecoder_)));
var $author$project$Components$Rules$Topic = F4(
	function (name, constants, functions, rules) {
		return {C: constants, y: functions, a: name, bd: rules};
	});
var $author$project$Components$Rules$Rule = F4(
	function (title, description, parameters, matches) {
		return {ag: description, aT: matches, bb: parameters, a$: title};
	});
var $author$project$Algo$Matcher$AnyMatcher = function (a) {
	return {$: 0, a: a};
};
var $author$project$Algo$Matcher$CommutativeMatcher = function (a) {
	return {$: 3, a: a};
};
var $author$project$Algo$Matcher$DeclarativeMatcher = function (a) {
	return {$: 4, a: a};
};
var $author$project$Algo$Matcher$ExactMatcher = function (a) {
	return {$: 2, a: a};
};
var $author$project$Algo$Matcher$RealMatcher = function (a) {
	return {$: 1, a: a};
};
var $author$project$Algo$Matcher$treeToMatcher_ = F4(
	function (checker, knownProps, state, root) {
		var processChildren = A2(
			$author$project$Helper$resultList,
			F2(
				function (child, _v12) {
					var list = _v12.a;
					var s = _v12.b;
					return A2(
						$elm$core$Result$map,
						function (_v13) {
							var newChild = _v13.a;
							var newS = _v13.b;
							return _Utils_Tuple2(
								A2($elm$core$List$cons, newChild, list),
								newS);
						},
						A4($author$project$Algo$Matcher$treeToMatcher_, checker, knownProps, s, child));
				}),
			_Utils_Tuple2(_List_Nil, state));
		switch (root.$) {
			case 0:
				var s = root.a;
				return $elm$core$Result$Ok(
					_Utils_Tuple2(
						$author$project$Algo$Matcher$RealMatcher(
							{x: s.x}),
						state));
			case 1:
				var s = root.a;
				var _v1 = A2($elm$core$Dict$get, s.a, knownProps);
				if (_v1.$ === 1) {
					return A2(
						$elm$core$Result$map,
						function (newS) {
							return _Utils_Tuple2(
								$author$project$Algo$Matcher$AnyMatcher(
									{e: _List_Nil, a: s.a}),
								newS);
						},
						A3(checker, s.a, 0, state));
				} else {
					var p = _v1.a;
					return (!(!p.e)) ? $elm$core$Result$Err(s.a + ' cannot be used as a variable') : $elm$core$Result$Ok(
						_Utils_Tuple2(
							$author$project$Algo$Matcher$ExactMatcher(
								{e: _List_Nil, a: s.a}),
							state));
				}
			case 2:
				var s = root.a;
				return A2(
					$elm$core$Result$map,
					function (_v2) {
						var childMatcher = _v2.a;
						var newS = _v2.b;
						return _Utils_Tuple2(
							$author$project$Algo$Matcher$ExactMatcher(
								{
									e: _List_fromArray(
										[childMatcher]),
									a: s.a
								}),
							newS);
					},
					A4($author$project$Algo$Matcher$treeToMatcher_, checker, knownProps, state, s.N));
			case 3:
				var s = root.a;
				return s.g ? A2(
					$elm$core$Result$map,
					function (_v3) {
						var children = _v3.a;
						var newS = _v3.b;
						return _Utils_Tuple2(
							$author$project$Algo$Matcher$CommutativeMatcher(
								{
									e: $elm$core$List$reverse(children),
									a: s.a,
									ay: $elm$core$Maybe$Nothing
								}),
							newS);
					},
					processChildren(s.af)) : A2(
					$elm$core$Result$map,
					function (_v4) {
						var children = _v4.a;
						var newS = _v4.b;
						return _Utils_Tuple2(
							$author$project$Algo$Matcher$ExactMatcher(
								{
									e: $elm$core$List$reverse(children),
									a: s.a
								}),
							newS);
					},
					processChildren(s.af));
			case 5:
				var s = root.a;
				return A2(
					$elm$core$Result$map,
					function (_v5) {
						var children = _v5.a;
						var newS = _v5.b;
						return _Utils_Tuple2(
							$author$project$Algo$Matcher$DeclarativeMatcher(
								{
									e: $elm$core$List$reverse(children),
									g: true,
									a: s.a
								}),
							newS);
					},
					processChildren(s.af));
			default:
				var s = root.a;
				var _v6 = A2($elm$core$Dict$get, s.a, knownProps);
				if (_v6.$ === 1) {
					return A2(
						$elm$core$Result$map,
						function (_v9) {
							var children = _v9.a;
							var newState = _v9.c;
							return _Utils_Tuple2(
								$author$project$Algo$Matcher$AnyMatcher(
									{
										e: $elm$core$List$reverse(children),
										a: s.a
									}),
								newState);
						},
						A2(
							$elm$core$Result$andThen,
							function (newState) {
								return A3(
									$author$project$Helper$resultList,
									F2(
										function (child, _v7) {
											var list = _v7.a;
											var seen = _v7.b;
											var newS = _v7.c;
											if (child.$ === 1) {
												var n = child.a;
												return A2($elm$core$Set$member, n.a, seen) ? $elm$core$Result$Err('Function argument can only match on unique variables') : A2(
													$elm$core$Result$map,
													function (finalS) {
														return _Utils_Tuple3(
															A2($elm$core$List$cons, n.a, list),
															A2($elm$core$Set$insert, n.a, seen),
															finalS);
													},
													A3(checker, n.a, 0, newS));
											} else {
												return $elm$core$Result$Err('Only variables are supported in function arguments');
											}
										}),
									_Utils_Tuple3(_List_Nil, $elm$core$Set$empty, newState),
									s.af);
							},
							A3(
								checker,
								s.a,
								$elm$core$List$length(s.af),
								state)));
				} else {
					var prop = _v6.a;
					return (!_Utils_eq(
						prop.e,
						$elm$core$List$length(s.af))) ? $elm$core$Result$Err(s.a + ' has an unexpected function signature') : (prop.g ? A2(
						$elm$core$Result$map,
						function (_v10) {
							var children = _v10.a;
							var newS = _v10.b;
							return _Utils_Tuple2(
								$author$project$Algo$Matcher$CommutativeMatcher(
									{e: children, a: s.a, ay: $elm$core$Maybe$Nothing}),
								newS);
						},
						processChildren(s.af)) : A2(
						$elm$core$Result$map,
						function (_v11) {
							var children = _v11.a;
							var newS = _v11.b;
							return _Utils_Tuple2(
								$author$project$Algo$Matcher$ExactMatcher(
									{e: children, a: s.a}),
								newS);
						},
						processChildren(s.af)));
				}
		}
	});
var $author$project$Algo$Matcher$parseMatcher = F3(
	function (checker, knownProps, state) {
		return A2(
			$elm$core$Basics$composeR,
			$author$project$Algo$Math$parse,
			$elm$core$Result$andThen(
				A3($author$project$Algo$Matcher$treeToMatcher_, checker, knownProps, state)));
	});
var $author$project$Components$Rules$expressionDecoder_ = F2(
	function (funcProps, args) {
		var checkUnknowns = F3(
			function (name, numArgs, dict) {
				var _v2 = A2($elm$core$Dict$get, name, dict);
				if (_v2.$ === 1) {
					return $elm$core$Result$Ok(
						A3(
							$elm$core$Dict$insert,
							name,
							_Utils_Tuple2(0, true),
							dict));
				} else {
					var _v3 = _v2.a;
					var n = _v3.a;
					return _Utils_eq(n, numArgs) ? $elm$core$Result$Ok(
						A3(
							$elm$core$Dict$insert,
							name,
							_Utils_Tuple2(n, false),
							dict)) : (((!n) || (!numArgs)) ? $elm$core$Result$Err('Variable cannot be used as a function') : $elm$core$Result$Err('Functions has different number of inputs'));
				}
			});
		return A2(
			$elm$json$Json$Decode$andThen,
			function (str) {
				var _v0 = A4($author$project$Algo$Matcher$parseMatcher, checkUnknowns, funcProps, args, str);
				if (_v0.$ === 1) {
					var errStr = _v0.a;
					return $elm$json$Json$Decode$fail('\'' + (str + ('\' is not a valid expression: ' + errStr)));
				} else {
					var _v1 = _v0.a;
					var matcher = _v1.a;
					var newArgs = _v1.b;
					return $elm$json$Json$Decode$succeed(
						_Utils_Tuple2(
							{a: str, ar: matcher},
							newArgs));
				}
			},
			$elm$json$Json$Decode$string);
	});
var $author$project$Components$Rules$parameterDecoder_ = function (knownFuncs) {
	return A2(
		$elm$json$Json$Decode$andThen,
		A2(
			$elm$core$Basics$composeR,
			A2(
				$author$project$Helper$resultList,
				F2(
					function (_v0, _v1) {
						var key = _v0.a;
						var description = _v0.b;
						var others = _v1.a;
						var dict = _v1.b;
						return A2(
							$elm$core$Result$andThen,
							function (tree) {
								switch (tree.$) {
									case 1:
										var m = tree.a;
										var _v3 = A2($elm$core$Dict$get, m.a, dict);
										if (!_v3.$) {
											return $elm$core$Result$Err('Parameters are duplicated');
										} else {
											var _v4 = A2($elm$core$Dict$get, m.a, knownFuncs);
											if (!_v4.$) {
												return $elm$core$Result$Err('Known constants cannot be used as a parameter');
											} else {
												return $elm$core$Result$Ok(
													_Utils_Tuple2(
														A3(
															$elm$core$Dict$insert,
															m.a,
															{e: 0, ag: description, a: key},
															others),
														A3(
															$elm$core$Dict$insert,
															m.a,
															_Utils_Tuple2(0, false),
															dict)));
											}
										}
									case 4:
										var m = tree.a;
										var _v5 = A2($elm$core$Dict$get, m.a, dict);
										if (!_v5.$) {
											return $elm$core$Result$Err('Parameters are duplicated');
										} else {
											var _v6 = A2($elm$core$Dict$get, m.a, knownFuncs);
											if (!_v6.$) {
												return $elm$core$Result$Err('Known function cannot be used as a parameter');
											} else {
												return $elm$core$Result$Ok(
													_Utils_Tuple2(
														A3(
															$elm$core$Dict$insert,
															m.a,
															{
																e: $elm$core$List$length(m.af),
																ag: description,
																a: key
															},
															others),
														A3(
															$elm$core$Dict$insert,
															m.a,
															_Utils_Tuple2(
																$elm$core$List$length(m.af),
																false),
															dict)));
											}
										}
									default:
										return $elm$core$Result$Err('Parameters can only be variables or functions');
								}
							},
							$author$project$Algo$Math$parse(key));
					}),
				_Utils_Tuple2($elm$core$Dict$empty, $elm$core$Dict$empty)),
			$author$project$Helper$resultToDecoder),
		$elm$json$Json$Decode$keyValuePairs($elm$json$Json$Decode$string));
};
var $author$project$Algo$Matcher$toReplacement = F2(
	function (funcProps, argDict) {
		var convert = function (node) {
			switch (node.$) {
				case 0:
					var s = node.a;
					return $elm$core$Result$Ok(
						$author$project$Algo$Math$RealNode(
							{b: $elm$core$Maybe$Nothing, x: s.x}));
				case 1:
					var s = node.a;
					var _v1 = A2($elm$core$Dict$get, s.a, argDict);
					if (!_v1.$) {
						var _v2 = _v1.a;
						var args = _v2.a;
						var index = _v2.b;
						return (!(!args)) ? $elm$core$Result$Err(s.a + ' is not a variable') : $elm$core$Result$Ok(
							$author$project$Algo$Math$VariableNode(
								{
									a: '',
									b: $elm$core$Maybe$Just(index)
								}));
					} else {
						var _v3 = A2($elm$core$Dict$get, s.a, funcProps);
						if (_v3.$ === 1) {
							return $elm$core$Result$Err('Unable to construct variable ' + s.a);
						} else {
							var p = _v3.a;
							return (!(!p.e)) ? $elm$core$Result$Err(s.a + ' is not a variable') : $elm$core$Result$Ok(
								$author$project$Algo$Math$VariableNode(
									{a: s.a, b: $elm$core$Maybe$Nothing}));
						}
					}
				case 2:
					var s = node.a;
					return A2(
						$elm$core$Result$map,
						function (child) {
							return $author$project$Algo$Math$UnaryNode(
								{N: child, a: s.a, b: $elm$core$Maybe$Nothing});
						},
						convert(s.N));
				case 3:
					var s = node.a;
					return A2(
						$elm$core$Result$map,
						function (children) {
							return $author$project$Algo$Math$BinaryNode(
								{
									j: s.j,
									af: $elm$core$List$reverse(children),
									g: s.g,
									a: s.a,
									b: $elm$core$Maybe$Nothing
								});
						},
						A3(
							$author$project$Helper$resultList,
							F2(
								function (child, list) {
									return A2(
										$elm$core$Result$map,
										function (c) {
											return A2($elm$core$List$cons, c, list);
										},
										convert(child));
								}),
							_List_Nil,
							s.af));
				case 4:
					var s = node.a;
					var _v4 = A2($elm$core$Dict$get, s.a, argDict);
					if (!_v4.$) {
						var _v5 = _v4.a;
						var args = _v5.a;
						var index = _v5.b;
						return (!_Utils_eq(
							args,
							$elm$core$List$length(s.af))) ? $elm$core$Result$Err(s.a + ' has the wrong number of inputs') : A2(
							$elm$core$Result$map,
							function (children) {
								return $author$project$Algo$Math$GenericNode(
									{
										af: children,
										a: '',
										b: $elm$core$Maybe$Just(index)
									});
							},
							A3(
								$author$project$Helper$resultList,
								F2(
									function (child, list) {
										return A2(
											$elm$core$Result$map,
											function (c) {
												return A2($elm$core$List$cons, c, list);
											},
											convert(child));
									}),
								_List_Nil,
								s.af));
					} else {
						var _v6 = A2($elm$core$Dict$get, s.a, funcProps);
						if (_v6.$ === 1) {
							return $elm$core$Result$Err('Unable to construct function ' + s.a);
						} else {
							var p = _v6.a;
							return ((p.e === 2) && (p.g || p.j)) ? A2(
								$elm$core$Result$map,
								function (children) {
									return $author$project$Algo$Math$BinaryNode(
										{
											j: p.j,
											af: $elm$core$List$reverse(children),
											g: p.g,
											a: s.a,
											b: $elm$core$Maybe$Nothing
										});
								},
								A3(
									$author$project$Helper$resultList,
									F2(
										function (child, list) {
											return A2(
												$elm$core$Result$map,
												function (c) {
													return A2($elm$core$List$cons, c, list);
												},
												convert(child));
										}),
									_List_Nil,
									s.af)) : ((!_Utils_eq(
								p.e,
								$elm$core$List$length(s.af))) ? $elm$core$Result$Err(s.a + ' has the wrong number of inputs') : A2(
								$elm$core$Result$map,
								function (children) {
									return $author$project$Algo$Math$GenericNode(
										{
											af: $elm$core$List$reverse(children),
											a: s.a,
											b: $elm$core$Maybe$Nothing
										});
								},
								A3(
									$author$project$Helper$resultList,
									F2(
										function (child, list) {
											return A2(
												$elm$core$Result$map,
												function (c) {
													return A2($elm$core$List$cons, c, list);
												},
												convert(child));
										}),
									_List_Nil,
									s.af)));
						}
					}
				default:
					var s = node.a;
					return A2(
						$elm$core$Result$map,
						function (children) {
							return $author$project$Algo$Math$DeclarativeNode(
								{
									af: $elm$core$List$reverse(children),
									a: s.a,
									b: $elm$core$Maybe$Nothing
								});
						},
						A3(
							$author$project$Helper$resultList,
							F2(
								function (child, list) {
									return A2(
										$elm$core$Result$map,
										function (c) {
											return A2($elm$core$List$cons, c, list);
										},
										convert(child));
								}),
							_List_Nil,
							s.af));
			}
		};
		return A2(
			$elm$core$Basics$composeR,
			$author$project$Algo$Math$parse,
			$elm$core$Result$andThen(convert));
	});
var $author$project$Components$Rules$replacementDecoder_ = F2(
	function (knownFunc, args) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (str) {
				return function (argMap) {
					return $author$project$Helper$resultToDecoder(
						A2(
							$elm$core$Result$map,
							function (replacement) {
								return {a: str, ar: replacement};
							},
							A3(
								$author$project$Algo$Matcher$toReplacement,
								knownFunc,
								$elm$core$Dict$fromList(argMap),
								str)));
				}(
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (index, _v0) {
								var _var = _v0.a;
								var _v1 = _v0.b;
								var argNum = _v1.a;
								return _Utils_Tuple2(
									_var,
									_Utils_Tuple2(argNum, index));
							}),
						$elm$core$Dict$toList(args)));
			},
			$elm$json$Json$Decode$string);
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$Components$Rules$setOthers_ = F2(
	function (others, m) {
		switch (m.$) {
			case 0:
				return m;
			case 1:
				return m;
			case 2:
				var s = m.a;
				return $author$project$Algo$Matcher$ExactMatcher(
					_Utils_update(
						s,
						{
							e: A2(
								$elm$core$List$map,
								$author$project$Components$Rules$setOthers_(others),
								s.e)
						}));
			case 4:
				var s = m.a;
				return $author$project$Algo$Matcher$DeclarativeMatcher(
					_Utils_update(
						s,
						{
							e: A2(
								$elm$core$List$map,
								$author$project$Components$Rules$setOthers_(others),
								s.e)
						}));
			default:
				var s = m.a;
				var _v1 = s.ay;
				if (!_v1.$) {
					return $author$project$Algo$Matcher$CommutativeMatcher(
						_Utils_update(
							s,
							{
								e: A2(
									$elm$core$List$map,
									$author$project$Components$Rules$setOthers_(others),
									s.e)
							}));
				} else {
					return function (res) {
						if (res.$ === 1) {
							return $author$project$Algo$Matcher$CommutativeMatcher(
								_Utils_update(
									s,
									{
										e: A2(
											$elm$core$List$map,
											$author$project$Components$Rules$setOthers_(others),
											s.e)
									}));
						} else {
							var _v5 = res.a;
							var index = _v5.a;
							var name = _v5.b;
							return $author$project$Algo$Matcher$CommutativeMatcher(
								_Utils_update(
									s,
									{
										e: A2(
											$elm$core$List$map,
											$author$project$Components$Rules$setOthers_(others),
											_Utils_ap(
												A2($elm$core$List$take, index, s.e),
												A2($elm$core$List$drop, index + 1, s.e))),
										ay: $elm$core$Maybe$Just(name)
									}));
						}
					}(
						A3(
							$elm$core$List$foldl,
							F2(
								function (_v2, res) {
									var index = _v2.a;
									var n = _v2.b;
									if (!n.$) {
										var c = n.a;
										return ($elm$core$List$isEmpty(c.e) && A2($elm$core$Set$member, c.a, others)) ? $elm$core$Maybe$Just(
											_Utils_Tuple2(index, c.a)) : res;
									} else {
										return res;
									}
								}),
							$elm$core$Maybe$Nothing,
							A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, s.e)));
				}
		}
	});
var $author$project$Components$Rules$ruleDecoder_ = function (knownFuncs) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (_v0) {
			var title = _v0.a;
			var description = _v0.b;
			var _v1 = _v0.c;
			var parameters = _v1.a;
			var args = _v1.b;
			return A2(
				$elm$json$Json$Decode$map,
				A3($author$project$Components$Rules$Rule, title, description, parameters),
				A2(
					$elm$json$Json$Decode$field,
					'matches',
					$elm$json$Json$Decode$list(
						A2(
							$elm$json$Json$Decode$andThen,
							function (_v2) {
								var from = _v2.a;
								var newArgs = _v2.b;
								return A2(
									$elm$json$Json$Decode$map,
									function (to) {
										var otherSet = $elm$core$Set$fromList(
											A2(
												$elm$core$List$map,
												$elm$core$Tuple$first,
												A2(
													$elm$core$List$filter,
													function (_v3) {
														var _v4 = _v3.b;
														var oneUse = _v4.b;
														return oneUse;
													},
													$elm$core$Dict$toList(newArgs))));
										return {
											a3: _Utils_update(
												from,
												{
													ar: A2($author$project$Components$Rules$setOthers_, otherSet, from.ar)
												}),
											bj: to
										};
									},
									A2(
										$elm$json$Json$Decode$field,
										'to',
										A2($author$project$Components$Rules$replacementDecoder_, knownFuncs, newArgs)));
							},
							A2(
								$elm$json$Json$Decode$field,
								'from',
								A2($author$project$Components$Rules$expressionDecoder_, knownFuncs, args))))));
		},
		A4(
			$elm$json$Json$Decode$map3,
			F3(
				function (a, b, c) {
					return _Utils_Tuple3(a, b, c);
				}),
			A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$field, 'description', $elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$map,
				$elm$core$Maybe$withDefault(
					_Utils_Tuple2($elm$core$Dict$empty, $elm$core$Dict$empty)),
				$elm$json$Json$Decode$maybe(
					A2(
						$elm$json$Json$Decode$field,
						'parameters',
						$author$project$Components$Rules$parameterDecoder_(knownFuncs))))));
};
var $author$project$Components$Rules$topicDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (_v0) {
		var name = _v0.a;
		var functions = _v0.b;
		var vars = _v0.c;
		if (!_Utils_eq(
			$elm$core$Dict$size(
				A2($elm$core$Dict$diff, vars, functions)),
			$elm$core$Dict$size(vars))) {
			return $elm$json$Json$Decode$fail('Can\'t have a variable named as a function as well');
		} else {
			var knownProps = function (dict) {
				return A3(
					$elm$core$Dict$foldl,
					F2(
						function (k, _v2) {
							return A2(
								$elm$core$Dict$insert,
								k,
								{e: 0, j: false, g: false});
						}),
					dict,
					vars);
			}(
				A2(
					$elm$core$Dict$map,
					function (_v1) {
						return function ($) {
							return $.k;
						};
					},
					functions));
			return A2(
				$elm$json$Json$Decode$map,
				function (eqs) {
					return A4($author$project$Components$Rules$Topic, name, vars, functions, eqs);
				},
				A2(
					$elm$json$Json$Decode$field,
					'actions',
					$elm$json$Json$Decode$list(
						$author$project$Components$Rules$ruleDecoder_(knownProps))));
		}
	},
	A4(
		$elm$json$Json$Decode$map3,
		F3(
			function (a, b, c) {
				return _Utils_Tuple3(a, b, c);
			}),
		A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
		A2(
			$elm$json$Json$Decode$field,
			'functions',
			$elm$json$Json$Decode$dict($author$project$Components$Rules$functionDecoder_)),
		A2(
			$elm$json$Json$Decode$field,
			'constants',
			$elm$json$Json$Decode$dict(
				A2(
					$elm$json$Json$Decode$andThen,
					function (str) {
						return (A2($elm$core$String$left, 5, str) !== 'Math.') ? $elm$json$Json$Decode$fail('Only constants from Math are allowed') : (A2(
							$elm$core$String$all,
							$elm$core$Char$isAlphaNum,
							A2($elm$core$String$dropLeft, 5, str)) ? $elm$json$Json$Decode$succeed(str) : $elm$json$Json$Decode$fail('Unknown characters after \'Math.\''));
					},
					$elm$json$Json$Decode$string)))));
var $author$project$Components$Rules$decoder = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (f, c, t) {
			return {C: c, y: f, v: t};
		}),
	A2(
		$elm$json$Json$Decode$field,
		'functions',
		$elm$json$Json$Decode$dict(
			A3(
				$elm$json$Json$Decode$map2,
				$elm$core$Tuple$pair,
				A2($elm$json$Json$Decode$field, 'properties', $author$project$Components$Rules$functionDecoder_),
				A2($elm$json$Json$Decode$field, 'count', $elm$json$Json$Decode$int)))),
	A2(
		$elm$json$Json$Decode$field,
		'constants',
		$elm$json$Json$Decode$dict(
			A3(
				$elm$json$Json$Decode$map2,
				$elm$core$Tuple$pair,
				A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
				A2($elm$json$Json$Decode$field, 'count', $elm$json$Json$Decode$int)))),
	A2(
		$elm$json$Json$Decode$field,
		'topics',
		$elm$json$Json$Decode$dict(
			A2(
				$elm$json$Json$Decode$andThen,
				function (s) {
					switch (s) {
						case 'notInstalled':
							return A2(
								$elm$json$Json$Decode$map,
								$author$project$Components$Rules$NotInstalled_,
								A2($elm$json$Json$Decode$field, 'url', $elm$json$Json$Decode$string));
						case 'installed':
							return A3(
								$elm$json$Json$Decode$map2,
								$author$project$Components$Rules$Installed_,
								$elm$json$Json$Decode$maybe(
									A2($elm$json$Json$Decode$field, 'url', $elm$json$Json$Decode$string)),
								A2($elm$json$Json$Decode$field, 'topic', $author$project$Components$Rules$topicDecoder));
						default:
							return $elm$json$Json$Decode$fail('Unknown loadState: ' + s);
					}
				},
				A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string)))));
var $author$project$Components$Tutorial$decoder = $elm$json$Json$Decode$succeed($author$project$Components$Tutorial$init);
var $author$project$UI$Draggable$Coordinates = F4(
	function (left, right, top, bottom) {
		return {B: bottom, w: left, A: right, E: top};
	});
var $author$project$UI$Draggable$Model = F3(
	function (coordinates, drags, id) {
		return {q: coordinates, ah: drags, ci: id};
	});
var $author$project$UI$Draggable$decoder = A6(
	$elm$json$Json$Decode$map5,
	F5(
		function (l, r, t, b, id) {
			return A3(
				$author$project$UI$Draggable$Model,
				A4($author$project$UI$Draggable$Coordinates, l, r, t, b),
				$elm$core$Dict$empty,
				id);
		}),
	A2($elm$json$Json$Decode$field, 'left', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'right', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'top', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'bottom', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string));
var $author$project$UI$Menu$decoder = A2(
	$elm$json$Json$Decode$map,
	function (s) {
		return {
			V: $elm$core$Set$fromList(s)
		};
	},
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string));
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$UI$Notification$notificationDecoder_ = A2(
	$elm$json$Json$Decode$andThen,
	A2(
		$elm$core$Basics$composeR,
		A2(
			$author$project$Helper$resultDict,
			F3(
				function (index, val, map) {
					var dec = A2(
						$author$project$UI$Animation$decoder,
						$elm$json$Json$Decode$string,
						$author$project$UI$Notification$delayedDelete_(index));
					var _v0 = A2($elm$json$Json$Decode$decodeValue, dec, val);
					if (!_v0.$) {
						var notification = _v0.a;
						return $elm$core$Result$Ok(
							A3($elm$core$Dict$insert, index, notification, map));
					} else {
						return $elm$core$Result$Err(
							'failed to deserialise ' + $elm$core$String$fromInt(index));
					}
				}),
			$elm$core$Dict$empty),
		$author$project$Helper$resultToDecoder),
	$author$project$Helper$intDictDecoder($elm$json$Json$Decode$value));
var $author$project$UI$Notification$decoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (id, n) {
			return {o: id, M: n};
		}),
	A2($elm$json$Json$Decode$field, 'nextID', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'notifications', $author$project$UI$Notification$notificationDecoder_));
var $author$project$Algo$Matcher$replacementDecoder = $author$project$Algo$Math$decoder(
	$elm$json$Json$Decode$maybe($elm$json$Json$Decode$int));
var $author$project$Main$evalTypeDecoder_ = A2(
	$elm$json$Json$Decode$andThen,
	function (t) {
		switch (t) {
			case 'numSub':
				return A5(
					$elm$json$Json$Decode$map4,
					$author$project$Main$NumSubType_,
					A2($elm$json$Json$Decode$field, 'eq', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'node', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'target', $elm$json$Json$Decode$float),
					A2($elm$json$Json$Decode$field, 'replacement', $author$project$Algo$Matcher$replacementDecoder));
			case 'eval':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Main$EvalType_,
					A2($elm$json$Json$Decode$field, 'eq', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'node', $elm$json$Json$Decode$int));
			default:
				return $elm$json$Json$Decode$fail('unknown type');
		}
	},
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
var $author$project$Main$triplet = F3(
	function (x, y, z) {
		return _Utils_Tuple3(x, y, z);
	});
var $author$project$Main$swappableDecoder = A2(
	$elm$json$Json$Decode$map,
	function (_v0) {
		var _v1 = _v0.a;
		var display = _v1.a;
		var rules = _v1.b;
		var tutorial = _v1.c;
		var _v2 = _v0.b;
		var _v3 = _v2.a;
		var notification = _v3.a;
		var menu = _v3.b;
		var _v4 = _v2.b;
		var evaluator = _v4.a;
		var createMode = _v4.b;
		var _v5 = _v0.c;
		var _v6 = _v5.a;
		var nextCreateInt = _v6.a;
		var showHelp = _v6.b;
		var _v7 = _v5.b;
		var showMenu = _v7.a;
		var showHistory = _v7.b;
		var historyBox = _v7.c;
		return $author$project$Main$Swappable(display)(rules)(tutorial)(notification)(menu)(evaluator)(createMode)(nextCreateInt)(showHelp)(showMenu)(showHistory)(historyBox);
	},
	A4(
		$elm$json$Json$Decode$map3,
		$author$project$Main$triplet,
		A4(
			$elm$json$Json$Decode$map3,
			$author$project$Main$triplet,
			A2($elm$json$Json$Decode$field, 'display', $author$project$Components$Display$decoder),
			A2($elm$json$Json$Decode$field, 'rules', $author$project$Components$Rules$decoder),
			A2($elm$json$Json$Decode$field, 'tutorial', $author$project$Components$Tutorial$decoder)),
		A5(
			$elm$json$Json$Decode$map4,
			F4(
				function (a, b, c, d) {
					return _Utils_Tuple2(
						_Utils_Tuple2(a, b),
						_Utils_Tuple2(c, d));
				}),
			A2($elm$json$Json$Decode$field, 'notification', $author$project$UI$Notification$decoder),
			A2($elm$json$Json$Decode$field, 'menu', $author$project$UI$Menu$decoder),
			A2(
				$elm$json$Json$Decode$field,
				'evaluator',
				A2($author$project$Components$Evaluate$decoder, $author$project$Main$evaluateString, $author$project$Main$evalTypeDecoder_)),
			$elm$json$Json$Decode$maybe(
				A2($elm$json$Json$Decode$field, 'createMode', $author$project$Main$createModeDecoder_))),
		A6(
			$elm$json$Json$Decode$map5,
			F5(
				function (a, b, c, d, e) {
					return _Utils_Tuple2(
						_Utils_Tuple2(a, b),
						_Utils_Tuple3(c, d, e));
				}),
			A2($elm$json$Json$Decode$field, 'nextCreateInt', $elm$json$Json$Decode$int),
			A2($elm$json$Json$Decode$field, 'showHelp', $elm$json$Json$Decode$bool),
			A2($elm$json$Json$Decode$field, 'showMenu', $elm$json$Json$Decode$bool),
			A2($elm$json$Json$Decode$field, 'showHistory', $elm$json$Json$Decode$bool),
			A2($elm$json$Json$Decode$field, 'historyBox', $author$project$UI$Draggable$decoder))));
var $elm$file$File$toString = _File_toString;
var $author$project$Algo$History$undo = function (model) {
	var _v0 = model.I;
	if (!_v0.b) {
		return model;
	} else {
		if (!_v0.b.b) {
			return model;
		} else {
			var x = _v0.a;
			var others = _v0.b;
			return _Utils_update(
				model,
				{
					ad: A2($elm$core$List$cons, x, model.ad),
					I: others
				});
		}
	}
};
var $author$project$Components$Display$undo = function (model) {
	var _v0 = model.n;
	if (_v0.$ === 1) {
		return $elm$core$Result$Err('No equation selected to undo');
	} else {
		var _v1 = _v0.a;
		var eqNum = _v1.a;
		var selected = _v1.b;
		var _v2 = A2($elm$core$Dict$get, eqNum, model.f);
		if (_v2.$ === 1) {
			return $elm$core$Result$Err('Equation not found to undo');
		} else {
			var his = _v2.a;
			var newHis = $author$project$Algo$History$undo(his);
			return $elm$core$Result$Ok(
				_Utils_update(
					model,
					{
						f: A3($elm$core$Dict$insert, eqNum, newHis, model.f),
						n: $elm$core$Maybe$Just(
							_Utils_Tuple2(
								eqNum,
								A2(
									$author$project$Components$Display$newSelectedNodes_,
									selected,
									$author$project$Algo$History$current(newHis))))
					}));
		}
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$Algo$Matcher$ungroupChild_ = F4(
	function (tracker, id, nodes, subEq) {
		var _v0 = subEq.ar;
		if (_v0.$ === 3) {
			var n = _v0.a;
			if (!n.j) {
				return $elm$core$Result$Err('Node is not associative');
			} else {
				var updateParent = function (s) {
					return A3(
						$elm$core$List$foldl,
						function (c) {
							return A2(
								$elm$core$Dict$insert,
								$author$project$Algo$Matcher$getID(
									$author$project$Algo$Math$getState(c)),
								id);
						},
						A2(
							$elm$core$Dict$remove,
							$author$project$Algo$Matcher$getID(s.b),
							tracker.aJ),
						s.af);
				};
				var traverseRemaining = F2(
					function (unselectedFound, children) {
						if (!children.b) {
							return $elm$core$Result$Err('All children are ungrouped');
						} else {
							var c = children.a;
							var other = children.b;
							if (c.$ === 3) {
								var m = c.a;
								if (!_Utils_eq(m.a, n.a)) {
									return A2(
										$elm$core$Result$map,
										function (_v3) {
											var list = _v3.a;
											var t = _v3.b;
											return _Utils_Tuple2(
												A2($elm$core$List$cons, c, list),
												t);
										},
										A2(traverseRemaining, unselectedFound, other));
								} else {
									if (A2(
										$elm$core$Set$member,
										$author$project$Algo$Matcher$getID(m.b),
										nodes)) {
										return $elm$core$Result$Ok(
											_Utils_Tuple2(
												_Utils_ap(m.af, other),
												_Utils_update(
													tracker,
													{
														aJ: updateParent(m)
													})));
									} else {
										var _v4 = A2(traverseRemaining, true, other);
										if (!_v4.$) {
											var _v5 = _v4.a;
											var list = _v5.a;
											var t = _v5.b;
											return $elm$core$Result$Ok(
												_Utils_Tuple2(
													A2($elm$core$List$cons, c, list),
													t));
										} else {
											var errStr = _v4.a;
											return unselectedFound ? $elm$core$Result$Err(errStr) : $elm$core$Result$Ok(
												_Utils_Tuple2(
													_Utils_ap(m.af, other),
													_Utils_update(
														tracker,
														{
															aJ: updateParent(m)
														})));
										}
									}
								}
							} else {
								return A2(
									$elm$core$Result$map,
									function (_v6) {
										var list = _v6.a;
										var t = _v6.b;
										return _Utils_Tuple2(
											A2($elm$core$List$cons, c, list),
											t);
									},
									A2(traverseRemaining, unselectedFound, other));
							}
						}
					});
				return A2(
					$elm$core$Result$map,
					function (_v7) {
						var list = _v7.a;
						var t = _v7.b;
						return _Utils_Tuple2(
							0,
							{
								ar: $author$project$Algo$Math$BinaryNode(
									_Utils_update(
										n,
										{af: list})),
								m: t
							});
					},
					A2(traverseRemaining, false, n.af));
			}
		} else {
			return $elm$core$Result$Err('Node is not associative');
		}
	});
var $author$project$Algo$Matcher$ungroupSubtree = F3(
	function (id, nodes, eq) {
		return A2(
			$elm$core$Result$map,
			$elm$core$Tuple$second,
			A3(
				$author$project$Algo$Matcher$processSubtree_,
				A2($author$project$Algo$Matcher$searchPath_, eq.m.aJ, id),
				A3($author$project$Algo$Matcher$ungroupChild_, eq.m, id, nodes),
				eq));
	});
var $author$project$Components$Display$ungroupChildren = F4(
	function (eqNum, id, selected, model) {
		var _v0 = A2($elm$core$Dict$get, eqNum, model.f);
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('Equation not found');
		} else {
			var eq = _v0.a;
			return A2(
				$elm$core$Result$map,
				function (newEq) {
					return _Utils_update(
						model,
						{
							f: A3(
								$elm$core$Dict$insert,
								eqNum,
								A2($author$project$Algo$History$add, newEq, eq),
								model.f),
							n: $elm$core$Maybe$Just(
								_Utils_Tuple2(
									eqNum,
									$elm$core$Set$singleton(id)))
						});
				},
				A3(
					$author$project$Algo$Matcher$ungroupSubtree,
					id,
					selected,
					$author$project$Algo$History$current(eq)));
		}
	});
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$remove, key, dict);
	});
var $author$project$Algo$History$update = F2(
	function (e, model) {
		var n = e;
		return _Utils_update(
			model,
			{
				I: A2($elm$core$List$cons, n, model.I)
			});
	});
var $author$project$Components$Display$update = F2(
	function (event, model) {
		switch (event.$) {
			case 0:
				var eq = event.a;
				var node = event.b;
				var _v1 = model.n;
				if (_v1.$ === 1) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								n: $elm$core$Maybe$Just(
									_Utils_Tuple2(
										eq,
										$elm$core$Set$singleton(node)))
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var _v2 = _v1.a;
					var e = _v2.a;
					var current = _v2.b;
					if (!_Utils_eq(e, eq)) {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									n: $elm$core$Maybe$Just(
										_Utils_Tuple2(
											eq,
											$elm$core$Set$singleton(node)))
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						if (A2($elm$core$Set$member, node, current)) {
							var newSet = A2($elm$core$Set$remove, node, current);
							return $elm$core$Set$isEmpty(newSet) ? _Utils_Tuple2(
								_Utils_update(
									model,
									{n: $elm$core$Maybe$Nothing}),
								$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
								_Utils_update(
									model,
									{
										n: $elm$core$Maybe$Just(
											_Utils_Tuple2(eq, newSet))
									}),
								$elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										n: $elm$core$Maybe$Just(
											_Utils_Tuple2(
												eq,
												A2($elm$core$Set$insert, node, current)))
									}),
								$elm$core$Platform$Cmd$none);
						}
					}
				}
			case 1:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{n: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 2:
				var eq = event.a;
				return A2($elm$core$Set$member, eq, model.P) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							P: A2($elm$core$Set$remove, eq, model.P)
						}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{
							P: A2($elm$core$Set$insert, eq, model.P)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var eq = event.a;
				var he = event.b;
				var _v3 = A2($elm$core$Dict$get, eq, model.f);
				if (_v3.$ === 1) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var his = _v3.a;
					var newHis = A2($author$project$Algo$History$update, he, his);
					var newModel = _Utils_update(
						model,
						{
							f: A3($elm$core$Dict$insert, eq, newHis, model.f)
						});
					var _v4 = model.n;
					if (_v4.$ === 1) {
						return _Utils_Tuple2(newModel, $elm$core$Platform$Cmd$none);
					} else {
						var _v5 = _v4.a;
						var eqNum = _v5.a;
						var selected = _v5.b;
						return (!_Utils_eq(eqNum, eq)) ? _Utils_Tuple2(newModel, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
							_Utils_update(
								newModel,
								{
									n: $elm$core$Maybe$Just(
										_Utils_Tuple2(
											eqNum,
											A2(
												$author$project$Components$Display$newSelectedNodes_,
												selected,
												$author$project$Algo$History$current(newHis))))
								}),
							$elm$core$Platform$Cmd$none);
					}
				}
		}
	});
var $author$project$Components$Tutorial$update = F2(
	function (_v0, model) {
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$UI$Draggable$ReleaseCapture = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$UI$Draggable$SetCapture = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$UI$Draggable$diff = F2(
	function (_v0, _v1) {
		var fromX = _v0.a;
		var fromY = _v0.b;
		var toX = _v1.a;
		var toY = _v1.b;
		return _Utils_Tuple2(toX - fromX, toY - fromY);
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$UI$Draggable$update = F3(
	function (_v0, e, model) {
		var screenX = _v0.a;
		var screenY = _v0.b;
		switch (e.$) {
			case 0:
				var d = e.a;
				var pid = e.b;
				var p = e.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							ah: A3(
								$elm$core$Dict$insert,
								A2($elm$json$Json$Encode$encode, 0, pid),
								{bq: d, a3: p, bI: model.q},
								model.ah)
						}),
					$elm$core$Maybe$Just(
						A2($author$project$UI$Draggable$SetCapture, model.ci, pid)));
			case 1:
				var pid = e.a;
				var to = e.b;
				var _v2 = A2(
					$elm$core$Dict$get,
					A2($elm$json$Json$Encode$encode, 0, pid),
					model.ah);
				if (_v2.$ === 1) {
					return _Utils_Tuple2(model, $elm$core$Maybe$Nothing);
				} else {
					var start = _v2.a;
					var orig = start.bI;
					var _v3 = A2($author$project$UI$Draggable$diff, start.a3, to);
					var diffX = _v3.a;
					var diffY = _v3.b;
					var _v4 = _Utils_Tuple2((diffX * 100) / screenX, (diffY * 100) / screenY);
					var dx = _v4.a;
					var dy = _v4.b;
					var _v5 = start.bq;
					switch (_v5) {
						case 2:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: {B: orig.B + dy, w: orig.w + dx, A: orig.A + dx, E: orig.E + dy}
									}),
								$elm$core$Maybe$Nothing);
						case 0:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: _Utils_update(
											orig,
											{
												w: A2($elm$core$Basics$min, orig.A - 2, orig.w + dx)
											})
									}),
								$elm$core$Maybe$Nothing);
						case 1:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: _Utils_update(
											orig,
											{
												A: A2($elm$core$Basics$max, orig.w + 2, orig.A + dx)
											})
									}),
								$elm$core$Maybe$Nothing);
						case 3:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: _Utils_update(
											orig,
											{
												B: A2($elm$core$Basics$max, orig.E + 2, orig.B + dy)
											})
									}),
								$elm$core$Maybe$Nothing);
						case 4:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: _Utils_update(
											orig,
											{
												w: A2($elm$core$Basics$min, orig.A - 2, orig.w + dx),
												E: A2($elm$core$Basics$min, orig.B - 2, orig.E + dy)
											})
									}),
								$elm$core$Maybe$Nothing);
						case 5:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: _Utils_update(
											orig,
											{
												A: A2($elm$core$Basics$max, orig.w + 2, orig.A + dx),
												E: A2($elm$core$Basics$min, orig.B - 2, orig.E + dy)
											})
									}),
								$elm$core$Maybe$Nothing);
						case 6:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: _Utils_update(
											orig,
											{
												B: A2($elm$core$Basics$max, orig.E + 2, orig.B + dy),
												w: A2($elm$core$Basics$min, orig.A - 2, orig.w + dx)
											})
									}),
								$elm$core$Maybe$Nothing);
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										q: _Utils_update(
											orig,
											{
												B: A2($elm$core$Basics$max, orig.E + 2, orig.B + dy),
												A: A2($elm$core$Basics$max, orig.w + 2, orig.A + dx)
											})
									}),
								$elm$core$Maybe$Nothing);
					}
				}
			case 2:
				var pid = e.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							ah: A2(
								$elm$core$Dict$remove,
								A2($elm$json$Json$Encode$encode, 0, pid),
								model.ah)
						}),
					$elm$core$Maybe$Just(
						A2($author$project$UI$Draggable$ReleaseCapture, model.ci, pid)));
			default:
				return _Utils_Tuple2(model, $elm$core$Maybe$Nothing);
		}
	});
var $author$project$UI$Menu$update = F2(
	function (event, model) {
		var entry = event;
		return A2($elm$core$Set$member, entry, model.V) ? _Utils_update(
			model,
			{
				V: A2($elm$core$Set$remove, entry, model.V)
			}) : _Utils_update(
			model,
			{
				V: A2($elm$core$Set$insert, entry, model.V)
			});
	});
var $author$project$UI$Notification$update = F2(
	function (e, model) {
		if (!e.$) {
			var id = e.a;
			var _v1 = A2($elm$core$Dict$get, id, model.M);
			if (_v1.$ === 1) {
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			} else {
				var n = _v1.a;
				return function (_v2) {
					var elem = _v2.a;
					var cmd = _v2.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								M: A3($elm$core$Dict$insert, id, elem, model.M)
							}),
						cmd);
				}(
					$author$project$UI$Animation$delete(n));
			}
		} else {
			var id = e.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						M: A2($elm$core$Dict$remove, id, model.M)
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$updateMathJax = _Platform_outgoingPort(
	'updateMathJax',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$token = function (str) {
	return $elm$parser$Parser$Advanced$token(
		$elm$parser$Parser$toToken(str));
};
var $author$project$Algo$Math$validVariable = function (str) {
	return A2(
		$elm$core$Result$mapError,
		function (_v0) {
			return 'Inavlid variable name';
		},
		A2(
			$elm$parser$Parser$run,
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed($elm$core$Basics$identity),
							$elm$parser$Parser$token('\\')),
						$author$project$Algo$Math$tokenLongName_),
						$author$project$Algo$Math$tokenShortName_
					])),
			str));
};
var $author$project$Main$update = F2(
	function (event, core) {
		var model = core.H;
		var updateCore = function (newModel) {
			return _Utils_update(
				core,
				{H: newModel});
		};
		switch (event.$) {
			case 0:
				return _Utils_Tuple2(core, $elm$core$Platform$Cmd$none);
			case 1:
				return _Utils_Tuple2(core, $elm$core$Platform$Cmd$none);
			case 2:
				var e = event.a;
				var _v1 = A2($author$project$Components$Display$update, e, model.i);
				var dModel = _v1.a;
				var dCmd = _v1.b;
				return _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{i: dModel})),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2($elm$core$Platform$Cmd$map, $author$project$Main$DisplayEvent, dCmd),
								A2($author$project$Main$updateQuery_, core, dModel),
								$author$project$Main$updateMathJax(0)
							])));
			case 4:
				var e = event.a;
				var _v2 = A2($author$project$Components$Tutorial$update, e, model.aC);
				var tModel = _v2.a;
				var tCmd = _v2.b;
				return _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{aC: tModel})),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$TutorialEvent, tCmd));
			case 5:
				var e = event.a;
				var _v3 = A2($author$project$UI$Notification$update, e, model.aa);
				var nModel = _v3.a;
				var nCmd = _v3.b;
				return _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{aa: nModel})),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$NotificationEvent, nCmd));
			case 6:
				var e = event.a;
				return _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{
								ax: A2($author$project$UI$Menu$update, e, model.ax)
							})),
					$elm$core$Platform$Cmd$none);
			case 7:
				var e = event.a;
				if (e.$ === 1) {
					var de = e.a;
					var _v5 = A3($author$project$UI$Draggable$update, core.aX, de, model.av);
					var newBox = _v5.a;
					var action = _v5.b;
					return _Utils_Tuple2(
						updateCore(
							_Utils_update(
								model,
								{av: newBox})),
						$author$project$Main$actionToCapture_(action));
				} else {
					var de = e.a;
					var _v6 = A2($author$project$Components$Display$update, de, model.i);
					var dModel = _v6.a;
					var dCmd = _v6.b;
					return _Utils_Tuple2(
						updateCore(
							_Utils_update(
								model,
								{i: dModel})),
						$elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									A2($elm$core$Platform$Cmd$map, $author$project$Main$DisplayEvent, dCmd),
									A2($author$project$Main$updateQuery_, core, dModel),
									$author$project$Main$updateMathJax(0)
								])));
				}
			case 8:
				return _Utils_Tuple2(core, $elm$core$Platform$Cmd$none);
			case 9:
				var input = event.a;
				var _v7 = _Utils_Tuple3(input.bo, input.b_, input.a6);
				_v7$3:
				while (true) {
					switch (_v7.c) {
						case 'Escape':
							var _v8 = _Utils_Tuple2(core.t, model.D);
							if (!_v8.a.$) {
								return _Utils_Tuple2(
									_Utils_update(
										core,
										{t: $elm$core$Maybe$Nothing}),
									$elm$core$Platform$Cmd$none);
							} else {
								if (!_v8.b.$) {
									var m = _v8.b.a;
									return function (_v9) {
										var newM = _v9.a;
										var cmd = _v9.b;
										return _Utils_Tuple2(
											updateCore(
												_Utils_update(
													model,
													{
														D: $elm$core$Maybe$Just(newM),
														U: false
													})),
											cmd);
									}(
										$author$project$UI$Animation$delete(m));
								} else {
									return _Utils_Tuple2(
										updateCore(
											_Utils_update(
												model,
												{Y: !model.Y})),
										$elm$core$Platform$Cmd$none);
								}
							}
						case 'z':
							if (_v7.a) {
								if (!_v7.b) {
									var _v10 = $author$project$Components$Display$undo(model.i);
									if (_v10.$ === 1) {
										var errStr = _v10.a;
										return A2($author$project$Main$submitNotification_, core, errStr);
									} else {
										var display = _v10.a;
										return _Utils_Tuple2(
											updateCore(
												_Utils_update(
													model,
													{i: display})),
											$elm$core$Platform$Cmd$none);
									}
								} else {
									var _v11 = $author$project$Components$Display$redo(model.i);
									if (_v11.$ === 1) {
										var errStr = _v11.a;
										return A2($author$project$Main$submitNotification_, core, errStr);
									} else {
										var display = _v11.a;
										return _Utils_Tuple2(
											updateCore(
												_Utils_update(
													model,
													{i: display})),
											$elm$core$Platform$Cmd$none);
									}
								}
							} else {
								break _v7$3;
							}
						default:
							break _v7$3;
					}
				}
				return _Utils_Tuple2(core, $elm$core$Platform$Cmd$none);
			case 10:
				return A2(
					$elm$core$Maybe$withDefault,
					true,
					A2(
						$elm$core$Maybe$map,
						function ($) {
							return $.ce;
						},
						model.D)) ? _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{
								D: $elm$core$Maybe$Just(
									A2(
										$author$project$UI$Animation$newDeletable,
										$author$project$Main$uiCancelCmd_(model.am),
										model.am)),
								am: model.am + 1
							})),
					$author$project$Main$focusTextBar_('textInput')) : _Utils_Tuple2(
					core,
					$author$project$Main$focusTextBar_('textInput'));
			case 11:
				var _v12 = model.D;
				if (_v12.$ === 1) {
					return _Utils_Tuple2(core, $elm$core$Platform$Cmd$none);
				} else {
					var m = _v12.a;
					return function (_v13) {
						var newM = _v13.a;
						var cmd = _v13.b;
						return _Utils_Tuple2(
							updateCore(
								_Utils_update(
									model,
									{
										D: $elm$core$Maybe$Just(newM),
										U: false
									})),
							cmd);
					}(
						$author$project$UI$Animation$delete(m));
				}
			case 12:
				var num = event.a;
				var _v14 = model.D;
				if (_v14.$ === 1) {
					return _Utils_Tuple2(core, $elm$core$Platform$Cmd$none);
				} else {
					var m = _v14.a;
					return (!_Utils_eq(m.br, num)) ? _Utils_Tuple2(core, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
						updateCore(
							_Utils_update(
								model,
								{D: $elm$core$Maybe$Nothing})),
						$elm$core$Platform$Cmd$none);
				}
			case 13:
				var str = event.a;
				if (str === '') {
					return _Utils_Tuple2(
						updateCore(
							_Utils_update(
								model,
								{D: $elm$core$Maybe$Nothing, U: false})),
						$elm$core$Platform$Cmd$none);
				} else {
					var _v15 = A2(
						$elm$core$Result$map,
						A2($author$project$Algo$Matcher$parseEquation, $author$project$Components$Display$createState, $author$project$Components$Display$updateState),
						A2(
							$elm$core$Result$andThen,
							$author$project$Components$Rules$replaceGlobalVar(model.bd),
							$author$project$Algo$Math$parse(str)));
					if (!_v15.$) {
						var root = _v15.a;
						return function (dModel) {
							return _Utils_Tuple2(
								updateCore(
									_Utils_update(
										model,
										{D: $elm$core$Maybe$Nothing, i: dModel, U: false})),
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											A2($author$project$Main$updateQuery_, core, dModel),
											$author$project$Main$updateMathJax(0)
										])));
						}(
							A2($author$project$Components$Display$addEquation, root, model.i));
					} else {
						var err = _v15.a;
						return A2($author$project$Main$submitNotification_, core, err);
					}
				}
			case 14:
				return _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{U: !model.U})),
					$author$project$Main$focusTextBar_('textInput'));
			case 15:
				return _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{Y: !model.Y})),
					$elm$core$Platform$Cmd$none);
			case 16:
				return _Utils_Tuple2(
					core,
					$author$project$Main$saveFile(model));
			case 17:
				var d = event.a;
				return _Utils_Tuple2(
					_Utils_update(
						core,
						{
							t: $elm$core$Maybe$Just(
								_Utils_Tuple2(d, $elm$core$Maybe$Nothing))
						}),
					function () {
						var _v16 = d.aG;
						if (_v16.$ === 1) {
							return $elm$core$Platform$Cmd$none;
						} else {
							var name = _v16.a;
							return $author$project$Main$focusTextBar_(
								$author$project$UI$Dialog$fieldID(name));
						}
					}());
			case 18:
				return _Utils_Tuple2(
					_Utils_update(
						core,
						{t: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 19:
				var url = event.a;
				var result = event.b;
				if (result.$ === 1) {
					var err = result.a;
					return A2(
						$author$project$Main$submitNotification_,
						core,
						A2($author$project$Main$httpErrorToString_, url, err));
				} else {
					var topic = result.a;
					var _v18 = A2($author$project$Components$Rules$addTopic, topic, model.bd);
					if (_v18.$ === 1) {
						var errStr = _v18.a;
						return A2($author$project$Main$submitNotification_, core, errStr);
					} else {
						var rModel = _v18.a;
						return _Utils_Tuple2(
							updateCore(
								_Utils_update(
									model,
									{bd: rModel})),
							$elm$core$Platform$Cmd$none);
					}
				}
			case 20:
				var url = event.a;
				var result = event.b;
				if (result.$ === 1) {
					var err = result.a;
					return A2(
						$author$project$Main$submitNotification_,
						core,
						A2($author$project$Main$httpErrorToString_, url, err));
				} else {
					var source = result.a;
					return _Utils_Tuple2(
						updateCore(
							_Utils_update(
								model,
								{
									bd: A2($author$project$Components$Rules$addSources, source.v, model.bd)
								})),
						$elm$core$Platform$Cmd$none);
				}
			case 21:
				var fileType = event.a;
				return _Utils_Tuple2(
					core,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['application/json']),
						$author$project$Main$FileSelected(fileType)));
			case 22:
				var fileType = event.a;
				var file = event.b;
				return _Utils_Tuple2(
					_Utils_update(
						core,
						{t: $elm$core$Maybe$Nothing}),
					A2(
						$elm$core$Task$perform,
						$author$project$Main$FileLoaded(fileType),
						$elm$file$File$toString(file)));
			case 23:
				var fileType = event.a;
				var str = event.b;
				if (!fileType) {
					return function (result) {
						if (result.$ === 1) {
							var errStr = result.a;
							return A2($author$project$Main$submitNotification_, core, errStr);
						} else {
							var rModel = result.a;
							return _Utils_Tuple2(
								updateCore(
									_Utils_update(
										model,
										{bd: rModel})),
								$elm$core$Platform$Cmd$none);
						}
					}(
						A2(
							$elm$core$Result$andThen,
							function (topic) {
								return A2($author$project$Components$Rules$addTopic, topic, model.bd);
							},
							A2(
								$elm$core$Result$mapError,
								$elm$json$Json$Decode$errorToString,
								A2($elm$json$Json$Decode$decodeString, $author$project$Components$Rules$topicDecoder, str))));
				} else {
					return function (result) {
						if (result.$ === 1) {
							var errStr = result.a;
							return A2($author$project$Main$submitNotification_, core, errStr);
						} else {
							var s = result.a;
							return _Utils_Tuple2(
								_Utils_update(
									core,
									{H: s}),
								A2($author$project$Main$updateQuery_, core, s.i));
						}
					}(
						A2(
							$elm$core$Result$mapError,
							$elm$json$Json$Decode$errorToString,
							A2($elm$json$Json$Decode$decodeString, $author$project$Main$swappableDecoder, str)));
				}
			case 24:
				return _Utils_Tuple2(
					updateCore(
						_Utils_update(
							model,
							{az: !model.az})),
					$elm$core$Platform$Cmd$none);
			case 25:
				var width = event.a;
				var height = event.b;
				return _Utils_Tuple2(
					_Utils_update(
						core,
						{
							aX: _Utils_Tuple2(width, height)
						}),
					$elm$core$Platform$Cmd$none);
			case 3:
				var e = event.a;
				switch (e.$) {
					case 0:
						var p = e.a;
						if (($elm$core$List$length(p.aT) === 1) && $elm$core$Dict$isEmpty(p.bb)) {
							var _v24 = A2($author$project$Helper$listIndex, 0, p.aT);
							if (_v24.$ === 1) {
								return A2($author$project$Main$submitNotification_, core, 'Unable to extract the match');
							} else {
								var m = _v24.a;
								return A2($author$project$Main$applyChange_, m, core);
							}
						} else {
							return _Utils_Tuple2(
								_Utils_update(
									core,
									{
										t: $elm$core$Maybe$Just(
											_Utils_Tuple2(
												$author$project$Main$parameterDialog_(p),
												$elm$core$Maybe$Just(p)))
									}),
								$elm$core$Platform$Cmd$none);
						}
					case 1:
						var eqNum = e.a;
						var root = e.b;
						var children = e.c;
						var _v25 = A4($author$project$Components$Display$groupChildren, eqNum, root, children, model.i);
						if (_v25.$ === 1) {
							var errStr = _v25.a;
							return A2($author$project$Main$submitNotification_, core, errStr);
						} else {
							var dModel = _v25.a;
							return _Utils_Tuple2(
								updateCore(
									_Utils_update(
										model,
										{i: dModel})),
								A2($author$project$Main$updateQuery_, core, dModel));
						}
					case 2:
						var eqNum = e.a;
						var root = e.b;
						var selected = e.c;
						var _v26 = A4($author$project$Components$Display$ungroupChildren, eqNum, root, selected, model.i);
						if (_v26.$ === 1) {
							var errStr = _v26.a;
							return A2($author$project$Main$submitNotification_, core, errStr);
						} else {
							var dModel = _v26.a;
							return _Utils_Tuple2(
								updateCore(
									_Utils_update(
										model,
										{i: dModel})),
								A2($author$project$Main$updateQuery_, core, dModel));
						}
					case 4:
						var eqNum = e.a;
						var root = e.b;
						return _Utils_Tuple2(
							_Utils_update(
								core,
								{
									t: $elm$core$Maybe$Just(
										_Utils_Tuple2(
											A3($author$project$Main$substitutionDialog_, eqNum, root, model),
											$elm$core$Maybe$Nothing))
								}),
							$elm$core$Platform$Cmd$none);
					case 3:
						var eqNum = e.a;
						var root = e.b;
						var target = e.c;
						return _Utils_Tuple2(
							_Utils_update(
								core,
								{
									t: $elm$core$Maybe$Just(
										_Utils_Tuple2(
											A3($author$project$Main$numSubDialog_, eqNum, root, target),
											$elm$core$Maybe$Nothing))
								}),
							$elm$core$Platform$Cmd$none);
					case 5:
						var url = e.a;
						return _Utils_Tuple2(
							core,
							$elm$http$Http$get(
								{
									bu: A2(
										$elm$http$Http$expectJson,
										$author$project$Main$ProcessTopic(url),
										$author$project$Components$Rules$topicDecoder),
									b3: url
								}));
					case 6:
						var eq = e.a;
						var id = e.b;
						var evalStr = e.c;
						var _v27 = A3(
							$author$project$Components$Evaluate$send,
							A2($author$project$Main$EvalType_, eq, id),
							evalStr,
							model.O);
						var eModel = _v27.a;
						var cmd = _v27.b;
						return _Utils_Tuple2(
							updateCore(
								_Utils_update(
									model,
									{O: eModel})),
							cmd);
					default:
						var topicName = e.a;
						return _Utils_Tuple2(
							_Utils_update(
								core,
								{
									t: $elm$core$Maybe$Nothing,
									H: _Utils_update(
										model,
										{
											bd: A2($author$project$Components$Rules$deleteTopic, topicName, model.bd)
										})
								}),
							$elm$core$Platform$Cmd$none);
				}
			case 26:
				var params = event.a;
				var _v28 = core.t;
				if ((!_v28.$) && (!_v28.a.b.$)) {
					var _v29 = _v28.a;
					var existing = _v29.b.a;
					return function (result) {
						if (result.$ === 1) {
							var errStr = result.a;
							return A2($author$project$Main$submitNotification_, core, errStr);
						} else {
							var newParams = result.a;
							return A2($author$project$Main$applyChange_, newParams, core);
						}
					}(
						A2(
							$elm$core$Result$andThen,
							function (prev) {
								return A3(
									$author$project$Helper$resultDict,
									F3(
										function (k, v, r) {
											if (k === '_method') {
												return $elm$core$Result$Ok(r);
											} else {
												switch (v.$) {
													case 0:
														var val = v.a;
														return A2(
															$elm$core$Result$map,
															function (tree) {
																return _Utils_update(
																	r,
																	{
																		a3: A3($author$project$Algo$Matcher$addMatch, k, tree, r.a3)
																	});
															},
															A3(
																$author$project$Algo$Matcher$toReplacement,
																$author$project$Components$Rules$functionProperties(model.bd),
																$elm$core$Dict$empty,
																val));
													case 2:
														var args = v.a;
														var val = v.b;
														return A2(
															$elm$core$Result$map,
															function (tree) {
																return _Utils_update(
																	r,
																	{
																		a3: A3($author$project$Algo$Matcher$addMatch, k, tree, r.a3)
																	});
															},
															A2(
																$elm$core$Result$andThen,
																function (argDict) {
																	return A3(
																		$author$project$Algo$Matcher$toReplacement,
																		$author$project$Components$Rules$functionProperties(model.bd),
																		argDict,
																		val);
																},
																A3(
																	$author$project$Helper$resultList,
																	F2(
																		function (_v32, dict) {
																			var i = _v32.a;
																			var name = _v32.b;
																			return A2($elm$core$Dict$member, name, dict) ? $elm$core$Result$Err('Function arguments need to be unique in the function definition') : A2(
																				$elm$core$Result$map,
																				function (n) {
																					return A3(
																						$elm$core$Dict$insert,
																						n,
																						_Utils_Tuple2(0, i),
																						dict);
																				},
																				$author$project$Algo$Math$validVariable(name));
																		}),
																	$elm$core$Dict$empty,
																	A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, args))));
													default:
														return $elm$core$Result$Ok(r);
												}
											}
										}),
									prev,
									params);
							},
							A2(
								$elm$core$Result$fromMaybe,
								'Unable to find the match',
								function () {
									var _v30 = A2($elm$core$Dict$get, '_method', params);
									if ((!_v30.$) && (_v30.a.$ === 1)) {
										var n = _v30.a.a;
										return A2($author$project$Helper$listIndex, n, existing.aT);
									} else {
										return A2($author$project$Helper$listIndex, 0, existing.aT);
									}
								}())));
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							core,
							{t: $elm$core$Maybe$Nothing}),
						$elm$core$Platform$Cmd$none);
				}
			case 27:
				var origNum = event.a;
				var root = event.b;
				var eqNum = event.c;
				return _Utils_Tuple2(
					_Utils_update(
						core,
						{t: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 28:
				var eqNum = event.a;
				var root = event.b;
				var target = event.c;
				var str = event.d;
				var _v34 = A3(
					$author$project$Algo$Matcher$toReplacement,
					$author$project$Components$Rules$functionProperties(model.bd),
					$elm$core$Dict$empty,
					str);
				if (_v34.$ === 1) {
					var errStr = _v34.a;
					return A2($author$project$Main$submitNotification_, core, errStr);
				} else {
					var replacement = _v34.a;
					var _v35 = A2($author$project$Components$Rules$evaluateStr, model.bd, replacement);
					if (_v35.$ === 1) {
						var errStr = _v35.a;
						return A2($author$project$Main$submitNotification_, core, errStr);
					} else {
						var evalStr = _v35.a;
						var _v36 = A3(
							$author$project$Components$Evaluate$send,
							A4($author$project$Main$NumSubType_, eqNum, root, target, replacement),
							evalStr,
							model.O);
						var eModel = _v36.a;
						var cmd = _v36.b;
						return _Utils_Tuple2(
							updateCore(
								_Utils_update(
									model,
									{O: eModel})),
							cmd);
					}
				}
			default:
				var reply = event.a;
				var _v37 = A2($author$project$Components$Evaluate$finish, reply.ci, model.O);
				var eModel = _v37.a;
				var c = _v37.b;
				var newCore = updateCore(
					_Utils_update(
						model,
						{O: eModel}));
				if (c.$ === 1) {
					return A2($author$project$Main$submitNotification_, newCore, 'Unable to evaluate a string');
				} else {
					if (!c.a.$) {
						var _v39 = c.a;
						var eqNum = _v39.a;
						var root = _v39.b;
						var target = _v39.c;
						var replacement = _v39.d;
						if (!_Utils_eq(target, reply.x)) {
							return A2(
								$author$project$Main$submitNotification_,
								newCore,
								'Expression evaluates to: ' + ($elm$core$String$fromFloat(reply.x) + (', but expecting: ' + $elm$core$String$fromFloat(target))));
						} else {
							var _v40 = A5($author$project$Components$Display$replaceNumber, eqNum, root, target, replacement, model.i);
							if (_v40.$ === 1) {
								var errStr = _v40.a;
								return A2($author$project$Main$submitNotification_, newCore, errStr);
							} else {
								var dModel = _v40.a;
								return _Utils_Tuple2(
									_Utils_update(
										core,
										{
											t: $elm$core$Maybe$Nothing,
											H: _Utils_update(
												model,
												{i: dModel, O: eModel})
										}),
									A2($author$project$Main$updateQuery_, core, dModel));
							}
						}
					} else {
						var _v41 = c.a;
						var eqNum = _v41.a;
						var id = _v41.b;
						var _v42 = A4($author$project$Components$Display$replaceNodeWithNumber, eqNum, id, reply.x, model.i);
						if (_v42.$ === 1) {
							var errStr = _v42.a;
							return A2($author$project$Main$submitNotification_, newCore, errStr);
						} else {
							var dModel = _v42.a;
							return _Utils_Tuple2(
								_Utils_update(
									core,
									{
										H: _Utils_update(
											model,
											{i: dModel, O: eModel})
									}),
								A2($author$project$Main$updateQuery_, core, dModel));
						}
					}
				}
		}
	});
var $author$project$UI$Menu$Content = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$EnterCreateMode = {$: 10};
var $author$project$Main$FileSelect = function (a) {
	return {$: 21, a: a};
};
var $author$project$Main$HistoryEvent = function (a) {
	return {$: 7, a: a};
};
var $author$project$Main$MenuEvent = function (a) {
	return {$: 6, a: a};
};
var $author$project$Main$OpenDialog = function (a) {
	return {$: 17, a: a};
};
var $author$project$Main$RuleEvent = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$Save = {$: 16};
var $author$project$Main$SaveFile = 1;
var $author$project$UI$Menu$Section = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Main$ToggleHistory = {$: 24};
var $author$project$Main$ToggleMenu = {$: 15};
var $elm$html$Html$a = _VirtualDom_node('a');
var $author$project$UI$Dialog$Button = function (a) {
	return {$: 1, a: a};
};
var $author$project$Components$Rules$Download = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$TopicFile = 0;
var $author$project$Main$addTopicDialog_ = {
	aQ: $author$project$Main$CloseDialog,
	aG: $elm$core$Maybe$Just('url'),
	aW: _List_fromArray(
		[
			{
			aw: _List_fromArray(
				[
					_List_fromArray(
					[
						$author$project$UI$Dialog$Text(
						{ci: 'url'})
					])
				]),
			aB: 'Load from a URL'
		},
			{
			aw: _List_fromArray(
				[
					_List_fromArray(
					[
						$author$project$UI$Dialog$Button(
						{
							cg: $author$project$Main$FileSelect(0),
							aL: 'Select a file'
						})
					])
				]),
			aB: 'Load from a file'
		}
		]),
	a_: function (val) {
		var _v0 = A2($elm$core$Dict$get, 'url', val);
		if ((!_v0.$) && (!_v0.a.$)) {
			var a = _v0.a.a;
			return $author$project$Main$RuleEvent(
				$author$project$Components$Rules$Download(a));
		} else {
			return $author$project$Main$NoOp;
		}
	},
	a$: 'Add a new Topic'
};
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $author$project$UI$Icon$class = $elm$svg$Svg$Attributes$class;
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Main$SubmitEquation = function (a) {
	return {$: 13, a: a};
};
var $author$project$Main$ToggleHelp = {$: 14};
var $author$project$Helper$maybeGuard = F2(
	function (yes, item) {
		return yes ? $elm$core$Maybe$Just(item) : $elm$core$Maybe$Nothing;
	});
var $author$project$UI$Animation$class = function (element) {
	return A2(
		$author$project$Helper$maybeGuard,
		element.ce,
		$elm$html$Html$Attributes$class('deleting'));
};
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $author$project$UI$Icon$equation = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
			attr),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$text_,
				_List_Nil,
				_List_fromArray(
					[
						$elm$svg$Svg$text('f(x)')
					]))
			]));
};
var $elm$html$Html$form = _VirtualDom_node('form');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
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
var $author$project$UI$HtmlEvent$onClick = function (event) {
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
var $author$project$UI$HtmlEvent$onSubmitField = F2(
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
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$inputDiv = function (model) {
	return _Utils_Tuple2(
		'textbar' + $elm$core$String$fromInt(model.br),
		A2(
			$elm$html$Html$form,
			A2(
				$author$project$Helper$maybeAppend,
				$author$project$UI$Animation$class(model),
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('textbar'),
						A2($author$project$UI$HtmlEvent$onSubmitField, 'equation', $author$project$Main$SubmitEquation)
					])),
			_List_fromArray(
				[
					$author$project$UI$Icon$equation(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('help'),
							$author$project$UI$Icon$class('clickable'),
							$author$project$UI$Icon$class('helpable'),
							$author$project$UI$HtmlEvent$onClick($author$project$Main$ToggleHelp)
						])),
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('text'),
							$elm$html$Html$Attributes$name('equation'),
							$elm$html$Html$Attributes$id('textInput'),
							$elm$html$Html$Attributes$placeholder('Type an equation to solve')
						]),
					_List_Nil)
				])));
};
var $author$project$Components$Display$ToggleHide = function (a) {
	return {$: 2, a: a};
};
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $author$project$UI$Icon$eye_ = _List_fromArray(
	[
		A2(
		$elm$svg$Svg$path,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$d('M1 13A12 12 1 0 1 23 13'),
				$elm$svg$Svg$Attributes$stroke('currentColor'),
				$elm$svg$Svg$Attributes$strokeWidth('1'),
				$elm$svg$Svg$Attributes$fill('none')
			]),
		_List_Nil),
		A2(
		$elm$svg$Svg$path,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$d('M2 12A12 9 0 0 0 22 12'),
				$elm$svg$Svg$Attributes$stroke('currentColor'),
				$elm$svg$Svg$Attributes$strokeWidth('1'),
				$elm$svg$Svg$Attributes$fill('none')
			]),
		_List_Nil),
		A2(
		$elm$svg$Svg$circle,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$cx('12'),
				$elm$svg$Svg$Attributes$cy('11'),
				$elm$svg$Svg$Attributes$r('5'),
				$elm$svg$Svg$Attributes$stroke('currentColor'),
				$elm$svg$Svg$Attributes$strokeWidth('1'),
				$elm$svg$Svg$Attributes$fill('none')
			]),
		_List_Nil)
	]);
var $author$project$UI$Icon$hidden = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
			attr),
		A2(
			$elm$core$List$cons,
			A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d('M2 22 L 22 2'),
						$elm$svg$Svg$Attributes$stroke('currentColor'),
						$elm$svg$Svg$Attributes$strokeWidth('1'),
						$elm$svg$Svg$Attributes$fill('none')
					]),
				_List_Nil),
			$author$project$UI$Icon$eye_));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$UI$Icon$shown = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
			attr),
		$author$project$UI$Icon$eye_);
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Components$Display$menu = F2(
	function (convert, model) {
		return A2(
			$elm$core$List$map,
			function (_v0) {
				var num = _v0.a;
				var eq = _v0.b;
				return $author$project$UI$Menu$Content(
					_List_fromArray(
						[
							A2(
							$elm$html$Html$a,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('clickable'),
									$author$project$UI$HtmlEvent$onClick(
									convert(
										$author$project$Components$Display$ToggleHide(num)))
								]),
							_List_fromArray(
								[
									A2($elm$core$Set$member, num, model.P) ? $author$project$UI$Icon$hidden(_List_Nil) : $author$project$UI$Icon$shown(_List_Nil),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('space')
										]),
									_List_Nil),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$author$project$Algo$Math$toString(
												$author$project$Algo$History$current(eq).ar))
										]))
								]))
						]));
			},
			$elm$core$Dict$toList(model.f));
	});
var $author$project$Components$Tutorial$menu = F2(
	function (converter, model) {
		return A2(
			$author$project$UI$Menu$Section,
			{W: $elm$core$Maybe$Nothing, a: 'Tutorials'},
			_List_Nil);
	});
var $author$project$UI$Icon$menu = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
			attr),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d('M2 0H22Q24 0 24 2Q24 4 22 4H2Q0 4 0 2Q0 0 2 0Z'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$stroke('none')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d('M2 10H22Q24 10 24 12Q24 14 22 14H2Q0 14 0 12Q0 10 2 10Z'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$stroke('none')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d('M2 20H22Q24 20 24 22Q24 24 22 24H2Q0 24 0 22Q0 20 2 20Z'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$stroke('none')
					]),
				_List_Nil)
			]));
};
var $author$project$Components$Rules$Delete = function (a) {
	return {$: 7, a: a};
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $author$project$UI$Icon$download = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
			attr),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d('M12 18L6 12L10 12L10 4L14 4L14 12L18 12Z'),
						$elm$svg$Svg$Attributes$fill('currentColor'),
						$elm$svg$Svg$Attributes$stroke('none')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$d('M4 20H20'),
						$elm$svg$Svg$Attributes$fill('none'),
						$elm$svg$Svg$Attributes$stroke('currentColor'),
						$elm$svg$Svg$Attributes$strokeWidth('4')
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $author$project$Components$Rules$menuTopics = F2(
	function (converter, model) {
		return $elm$core$List$reverse(
			A3(
				$elm$core$Dict$foldl,
				F2(
					function (k, t) {
						return $elm$core$List$cons(
							function () {
								if (!t.$) {
									var url = t.a;
									return A2(
										$author$project$UI$Menu$Section,
										{
											W: $elm$core$Maybe$Just(
												$author$project$UI$Icon$download(
													_List_fromArray(
														[
															$author$project$UI$HtmlEvent$onClick(
															converter(
																$author$project$Components$Rules$Download(url))),
															$author$project$UI$Icon$class('clickable')
														]))),
											a: k
										},
										_List_fromArray(
											[
												$author$project$UI$Menu$Content(
												_List_fromArray(
													[
														A2(
														$elm$html$Html$p,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text('<Add text for what this topic is about>')
															]))
													]))
											]));
								} else {
									var topic = t.b;
									return A2(
										$author$project$UI$Menu$Section,
										{
											W: $elm$core$Maybe$Just(
												A2(
													$elm$html$Html$a,
													_List_fromArray(
														[
															$author$project$UI$HtmlEvent$onClick(
															converter(
																$author$project$Components$Rules$Delete(topic.a))),
															$elm$html$Html$Attributes$class('clickable')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text('x')
														]))),
											a: topic.a
										},
										A2(
											$elm$core$List$map,
											function (rule) {
												return A2(
													$author$project$UI$Menu$Section,
													{W: $elm$core$Maybe$Nothing, a: rule.a$},
													_List_fromArray(
														[
															$author$project$UI$Menu$Content(
															$elm$core$List$concat(
																_List_fromArray(
																	[
																		_List_fromArray(
																		[
																			A2(
																			$elm$html$Html$h3,
																			_List_Nil,
																			_List_fromArray(
																				[
																					$elm$html$Html$text('Rules')
																				]))
																		]),
																		A2(
																		$elm$core$List$map,
																		function (match) {
																			return A2(
																				$elm$html$Html$p,
																				_List_Nil,
																				_List_fromArray(
																					[
																						$elm$html$Html$text(match.a3.a + ('→' + match.bj.a))
																					]));
																		},
																		rule.aT),
																		_List_fromArray(
																		[
																			A2(
																			$elm$html$Html$h3,
																			_List_Nil,
																			_List_fromArray(
																				[
																					$elm$html$Html$text('Description')
																				])),
																			A2(
																			$elm$html$Html$p,
																			_List_Nil,
																			_List_fromArray(
																				[
																					$elm$html$Html$text(rule.ag)
																				]))
																		])
																	])))
														]));
											},
											topic.bd));
								}
							}());
					}),
				_List_fromArray(
					[
						A2(
						$author$project$UI$Menu$Section,
						{W: $elm$core$Maybe$Nothing, a: 'Core'},
						_List_fromArray(
							[
								A2(
								$author$project$UI$Menu$Section,
								{W: $elm$core$Maybe$Nothing, a: 'Evaluate'},
								_List_fromArray(
									[
										$author$project$UI$Menu$Content(
										_List_fromArray(
											[
												A2(
												$elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Convert expression into a single number')
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('If the section does not contain any unknown variables, then the calculator can crunch the numbers to return a value.')
													]))
											]))
									])),
								A2(
								$author$project$UI$Menu$Section,
								{W: $elm$core$Maybe$Nothing, a: 'Number Substitution'},
								_List_fromArray(
									[
										$author$project$UI$Menu$Content(
										_List_fromArray(
											[
												A2(
												$elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Given x=y, f(x)=f(y)')
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Modify the number based on some calculation. Use this to split the number up into small things, i.e. using 2+3=5 to make 5 into 2+3')
													]))
											]))
									])),
								A2(
								$author$project$UI$Menu$Section,
								{W: $elm$core$Maybe$Nothing, a: 'Substitute'},
								_List_fromArray(
									[
										$author$project$UI$Menu$Content(
										_List_fromArray(
											[
												A2(
												$elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Given x=y, f(x)=f(y)')
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Since the equation provided means that both sides have the same value, the statement will remain true when replacing all occurances with one by the other.')
													]))
											]))
									])),
								A2(
								$author$project$UI$Menu$Section,
								{W: $elm$core$Maybe$Nothing, a: 'Group'},
								_List_fromArray(
									[
										$author$project$UI$Menu$Content(
										_List_fromArray(
											[
												A2(
												$elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Focus on a specific part')
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Associative operators can be done in any order. These include Addition and Multiplication. The parts that are in focus will be brought together.')
													]))
											]))
									])),
								A2(
								$author$project$UI$Menu$Section,
								{W: $elm$core$Maybe$Nothing, a: 'Ungroup'},
								_List_fromArray(
									[
										$author$project$UI$Menu$Content(
										_List_fromArray(
											[
												A2(
												$elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Return the group with the rest')
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Associative operators can be done in any order. These include Additional and Multiplication. The parts that were in focus will be back with the other to see ')
													]))
											]))
									]))
							]))
					]),
				model.v));
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $author$project$Algo$Math$notation = '\na - a variable called "a"\nab - a variable "a" multiplied by a variable "b"\n\\ab - a variable with a long name called "ab"\n\\a(x,y;z) - a function called "a" that takes in x and y, and uses z\n';
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $author$project$Components$Display$Select = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Components$Display$collapsedView_ = F3(
	function (eq, highlight, node) {
		if (!node.$) {
			var val = node.a;
			return $elm$html$Html$text(val);
		} else {
			var s = node.a;
			var id = $author$project$Algo$Matcher$getID(s.b);
			return A2(
				$elm$html$Html$div,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('node'),
							$author$project$UI$HtmlEvent$onClick(
							A2($author$project$Components$Display$Select, eq, id))
						]),
					A2($elm$core$Set$member, id, highlight) ? _List_fromArray(
						[
							$elm$html$Html$Attributes$class('selected')
						]) : _List_Nil),
				A2(
					$elm$core$List$map,
					A2($author$project$Components$Display$collapsedView_, eq, highlight),
					s.af));
		}
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $author$project$UI$Block$strokeWidth_ = 0.08;
var $author$project$UI$Block$scaleHeight_ = function (height) {
	return height - $author$project$UI$Block$strokeWidth_;
};
var $author$project$UI$Block$horizontalPad_ = 0.1;
var $author$project$UI$Block$scaleWidth_ = function (width) {
	return ((width * (1 + $author$project$UI$Block$horizontalPad_)) - $author$project$UI$Block$horizontalPad_) - $author$project$UI$Block$strokeWidth_;
};
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$UI$Block$blocks = F3(
	function (xMax, yMax, children) {
		return A2(
			$elm$svg$Svg$svg,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$viewBox(
					'-0.5 ' + ($elm$core$String$fromFloat(
						(-$author$project$UI$Block$scaleHeight_(yMax)) - 0.5) + (' ' + ($elm$core$String$fromFloat(
						$author$project$UI$Block$scaleWidth_(xMax) + 1) + (' ' + $elm$core$String$fromFloat(
						$author$project$UI$Block$scaleHeight_(yMax) + 1)))))),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromInt(2 * (xMax + 1)) + 'rem'),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromInt(2 * (yMax + 1)) + 'rem'),
					$elm$svg$Svg$Attributes$class('blocks')
				]),
			children);
	});
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$UI$Block$block = F7(
	function (xMin, xMax, yMin, yMax, selected, onClick, name) {
		var y_ = -yMin;
		var x_ = xMin / (1 - $author$project$UI$Block$horizontalPad_);
		var width_ = $author$project$UI$Block$scaleWidth_(xMax - xMin);
		var height_ = $author$project$UI$Block$scaleHeight_(1);
		return A2(
			$elm$svg$Svg$g,
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$core$Maybe$Just(onClick),
						$elm$core$Maybe$Just(
						$elm$svg$Svg$Attributes$class('block')),
						A2(
						$author$project$Helper$maybeGuard,
						selected,
						$elm$svg$Svg$Attributes$class('selected'))
					])),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$rect,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(x_)),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(y_)),
							$elm$svg$Svg$Attributes$width(
							$elm$core$String$fromFloat(width_)),
							$elm$svg$Svg$Attributes$height(
							$elm$core$String$fromFloat(height_)),
							$elm$svg$Svg$Attributes$strokeWidth(
							$elm$core$String$fromFloat($author$project$UI$Block$strokeWidth_)),
							$elm$svg$Svg$Attributes$class('blockRect')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$text_,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$x(
							$elm$core$String$fromFloat(x_ + (width_ / 2))),
							$elm$svg$Svg$Attributes$y(
							$elm$core$String$fromFloat(y_ + (height_ / 2))),
							$elm$svg$Svg$Attributes$class('blockText')
						]),
					_List_fromArray(
						[
							$elm$svg$Svg$text(name)
						]))
				]));
	});
var $author$project$Algo$Math$getChildren = function (node) {
	switch (node.$) {
		case 0:
			return _List_Nil;
		case 1:
			return _List_Nil;
		case 2:
			var s = node.a;
			return _List_fromArray(
				[s.N]);
		case 3:
			var s = node.a;
			return s.af;
		case 4:
			var s = node.a;
			return s.af;
		default:
			var s = node.a;
			return s.af;
	}
};
var $author$project$Components$Display$stackRecursive = F5(
	function (eq, highlight, minWidth, minDepth, node) {
		var id = $author$project$Algo$Matcher$getID(
			$author$project$Algo$Math$getState(node));
		var onClick = $author$project$UI$HtmlEvent$onClick(
			A2($author$project$Components$Display$Select, eq, id));
		var selected = A2($elm$core$Set$member, id, highlight);
		var children = $author$project$Algo$Math$getChildren(node);
		var _v0 = $elm$core$List$isEmpty(children) ? _Utils_Tuple3(minWidth + 1, minDepth, _List_Nil) : A3(
			$elm$core$List$foldl,
			F2(
				function (child, _v1) {
					var foldWidth = _v1.a;
					var foldDepth = _v1.b;
					var foldDivs = _v1.c;
					var _v2 = A5($author$project$Components$Display$stackRecursive, eq, highlight, foldWidth, minDepth + 1, child);
					var w = _v2.a;
					var d = _v2.b;
					var divs = _v2.c;
					return _Utils_Tuple3(
						w,
						A2($elm$core$Basics$max, foldDepth, d),
						_Utils_ap(foldDivs, divs));
				}),
			_Utils_Tuple3(minWidth, minDepth, _List_Nil),
			children);
		var maxWidth = _v0.a;
		var maxDepth = _v0.b;
		var childBlocks = _v0.c;
		return _Utils_Tuple3(
			maxWidth,
			maxDepth,
			A2(
				$elm$core$List$cons,
				A7(
					$author$project$UI$Block$block,
					minWidth,
					maxWidth,
					minDepth,
					maxDepth,
					selected,
					onClick,
					$author$project$Algo$Math$getName(node)),
				childBlocks));
	});
var $author$project$Components$Display$stackedView_ = F3(
	function (eq, highlight, root) {
		var _v0 = A5($author$project$Components$Display$stackRecursive, eq, highlight, 0, 1, root);
		var maxWidth = _v0.a;
		var maxDepth = _v0.b;
		var blocks = _v0.c;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A3($author$project$UI$Block$blocks, maxWidth, maxDepth, blocks)
				]));
	});
var $author$project$Components$Display$view = F3(
	function (converter, attr, model) {
		return A2(
			$elm$html$Html$div,
			attr,
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (eqNum, hisEq, result) {
						if (A2($elm$core$Set$member, eqNum, model.P)) {
							return result;
						} else {
							var highlight = A2(
								$elm$core$Maybe$withDefault,
								$elm$core$Set$empty,
								A2(
									$elm$core$Maybe$andThen,
									function (_v0) {
										var selEq = _v0.a;
										var set = _v0.b;
										return _Utils_eq(selEq, eqNum) ? $elm$core$Maybe$Just(set) : $elm$core$Maybe$Nothing;
									},
									model.n));
							var eq = $author$project$Algo$History$current(hisEq);
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
												$author$project$Components$Display$collapsedView_,
												eqNum,
												highlight,
												$author$project$Algo$Math$symbolicate(eq.ar)),
												A3($author$project$Components$Display$stackedView_, eqNum, highlight, eq.ar)
											]))),
								result);
						}
					}),
				_List_Nil,
				model.f));
	});
var $author$project$UI$ActionView$coreToList_ = function (state) {
	return _List_fromArray(
		[
			_Utils_Tuple2('Evaluate', state.bt),
			_Utils_Tuple2('Number Substitution', state.a8),
			_Utils_Tuple2('Substitution', state.bi),
			_Utils_Tuple2('Group', state.a4),
			_Utils_Tuple2('Ungroup', state.bk)
		]);
};
var $author$project$UI$ActionView$Allowed = function (a) {
	return {$: 2, a: a};
};
var $author$project$UI$ActionView$CoreTopicState = F5(
	function (substitute, group, ungroup, numSubstitute, evaluate) {
		return {bt: evaluate, a4: group, a8: numSubstitute, bi: substitute, bk: ungroup};
	});
var $author$project$UI$ActionView$Disallowed = {$: 1};
var $author$project$UI$ActionView$DisplayOnly = {$: 0};
var $author$project$Components$Rules$Evaluate = F3(
	function (a, b, c) {
		return {$: 6, a: a, b: b, c: c};
	});
var $author$project$Components$Rules$Group = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$Components$Rules$NumericalSubstitution = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $author$project$Components$Rules$Substitute = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Components$Rules$Ungroup = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $author$project$UI$ActionView$coreTopic_ = F2(
	function (rModel, selection) {
		if (selection.$ === 1) {
			return A5($author$project$UI$ActionView$CoreTopicState, $author$project$UI$ActionView$DisplayOnly, $author$project$UI$ActionView$DisplayOnly, $author$project$UI$ActionView$DisplayOnly, $author$project$UI$ActionView$DisplayOnly, $author$project$UI$ActionView$DisplayOnly);
		} else {
			var selected = selection.a;
			var evaluateState = function () {
				var _v3 = A2($author$project$Components$Rules$evaluateStr, rModel, selected.aN);
				if (_v3.$ === 1) {
					return $author$project$UI$ActionView$Disallowed;
				} else {
					var str = _v3.a;
					return $author$project$UI$ActionView$Allowed(
						A3($author$project$Components$Rules$Evaluate, selected.ai, selected.ar, str));
				}
			}();
			var result = A5(
				$author$project$UI$ActionView$CoreTopicState,
				$author$project$UI$ActionView$Allowed(
					A2($author$project$Components$Rules$Substitute, selected.ai, selected.ar)),
				$author$project$UI$ActionView$Disallowed,
				$author$project$UI$ActionView$Disallowed,
				$author$project$UI$ActionView$Disallowed,
				evaluateState);
			var _v1 = selected.aN;
			switch (_v1.$) {
				case 3:
					var n = _v1.a;
					if (!n.j) {
						return result;
					} else {
						var selectedChildren = $elm$core$List$length(
							A2(
								$elm$core$List$filter,
								function (child) {
									return A2(
										$elm$core$Set$member,
										$author$project$Algo$Matcher$getID(
											$author$project$Algo$Math$getState(child)),
										selected.R);
								},
								n.af));
						var sameBinaryNode = A2(
							$elm$core$List$any,
							function (child) {
								if (child.$ === 3) {
									var m = child.a;
									return _Utils_eq(n.a, m.a);
								} else {
									return false;
								}
							},
							n.af);
						var ungroupRes = sameBinaryNode ? _Utils_update(
							result,
							{
								bk: $author$project$UI$ActionView$Allowed(
									A3($author$project$Components$Rules$Ungroup, selected.ai, selected.ar, selected.R))
							}) : result;
						return (_Utils_eq(
							$elm$core$List$length(n.af),
							selectedChildren) || (selectedChildren < 2)) ? ungroupRes : _Utils_update(
							ungroupRes,
							{
								a4: $author$project$UI$ActionView$Allowed(
									A3($author$project$Components$Rules$Group, selected.ai, selected.ar, selected.R))
							});
					}
				case 0:
					var n = _v1.a;
					return _Utils_update(
						result,
						{
							a8: $author$project$UI$ActionView$Allowed(
								A3($author$project$Components$Rules$NumericalSubstitution, selected.ai, selected.ar, n.x)),
							bi: $author$project$UI$ActionView$Disallowed
						});
				default:
					return result;
			}
		}
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$li = _VirtualDom_node('li');
var $author$project$UI$ActionView$displayTopic_ = F2(
	function (title, actions) {
		return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (_v0) {
							var name = _v0.a;
							var state = _v0.b;
							return A2(
								$elm$html$Html$a,
								function () {
									switch (state.$) {
										case 0:
											return _List_Nil;
										case 1:
											return _List_fromArray(
												[
													A2($elm$html$Html$Attributes$attribute, 'disabled', '')
												]);
										default:
											var event = state.a;
											return _List_fromArray(
												[
													$author$project$UI$HtmlEvent$onClick(event),
													$elm$html$Html$Attributes$class('clickable')
												]);
									}
								}(),
								_List_fromArray(
									[
										$elm$html$Html$text(name)
									]));
						},
						actions))
				]));
	});
var $author$project$Components$Rules$loadedTopics = function (model) {
	return A2(
		$elm$core$List$filterMap,
		function (_v0) {
			var state = _v0.b;
			if (state.$ === 1) {
				var obj = state.b;
				return $elm$core$Maybe$Just(obj);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		},
		$elm$core$Dict$toList(model.v));
};
var $author$project$Components$Rules$Apply = function (a) {
	return {$: 0, a: a};
};
var $author$project$Algo$Matcher$InternalValue_ = function (a) {
	return {$: 1, a: a};
};
var $author$project$Algo$Backtrack$fail = $elm$core$Maybe$Nothing;
var $author$project$Algo$Backtrack$Choice_ = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Algo$Backtrack$getState = function (c) {
	return A2($elm$core$Maybe$map, $elm$core$Tuple$first, c.p);
};
var $author$project$Algo$Backtrack$orderedStack = function (func) {
	return $elm$core$List$map(
		F3(
			function (s, choices, tok) {
				return A2(
					$elm$core$Maybe$andThen,
					function (newChild) {
						return A2(
							$elm$core$Maybe$map,
							function (newRes) {
								return _Utils_update(
									tok,
									{
										p: $elm$core$Maybe$Just(
											_Utils_Tuple2(
												newRes,
												A2($author$project$Algo$Backtrack$Choice_, 0, newChild)))
									});
							},
							$author$project$Algo$Backtrack$getState(newChild));
					},
					A2(
						$elm$core$Maybe$andThen,
						function (c) {
							var _v0 = tok.p;
							if (((!_v0.$) && (_v0.a.b.$ === 1)) && (!_v0.a.b.a)) {
								var _v1 = _v0.a;
								var _v2 = _v1.b;
								var childTok = _v2.b;
								return A3(func, s, c, childTok);
							} else {
								return $elm$core$Maybe$Nothing;
							}
						},
						$elm$core$List$head(choices)));
			}));
};
var $author$project$Algo$Matcher$MultiValue_ = function (a) {
	return {$: 2, a: a};
};
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $author$project$Algo$Backtrack$Match_ = {$: 0};
var $author$project$Algo$Backtrack$return = F2(
	function (func, token) {
		var _v0 = token.p;
		if (_v0.$ === 1) {
			return A2(
				$elm$core$Maybe$map,
				function (newS) {
					return _Utils_update(
						token,
						{
							p: $elm$core$Maybe$Just(
								_Utils_Tuple2(newS, $author$project$Algo$Backtrack$Match_))
						});
				},
				func(token.aS));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Algo$Matcher$toInternalReplacement_ = function (argDict) {
	return A2(
		$elm$core$Basics$composeR,
		A2(
			$author$project$Algo$Math$map,
			F3(
				function (_v0, node, _v1) {
					switch (node.$) {
						case 1:
							var s = node.a;
							var _v3 = A2($elm$core$Dict$get, s.a, argDict);
							if (_v3.$ === 1) {
								return _Utils_Tuple2(
									_Utils_Tuple2(s.b, $elm$core$Maybe$Nothing),
									0);
							} else {
								var n = _v3.a;
								return _Utils_Tuple2(
									_Utils_Tuple2(
										s.b,
										$elm$core$Maybe$Just(n)),
									0);
							}
						case 4:
							var s = node.a;
							var _v4 = A2($elm$core$Dict$get, s.a, argDict);
							if (_v4.$ === 1) {
								return _Utils_Tuple2(
									_Utils_Tuple2(s.b, $elm$core$Maybe$Nothing),
									0);
							} else {
								var n = _v4.a;
								return _Utils_Tuple2(
									_Utils_Tuple2(
										s.b,
										$elm$core$Maybe$Just(n)),
									0);
							}
						default:
							return _Utils_Tuple2(
								_Utils_Tuple2(
									$author$project$Algo$Math$getState(node),
									$elm$core$Maybe$Nothing),
								0);
					}
				}),
			0),
		$elm$core$Tuple$first);
};
var $author$project$Algo$Matcher$otherMatchEvaluator_ = F5(
	function (op, associative, commutative, key, nodes) {
		return $author$project$Algo$Backtrack$return(
			function (result) {
				return $elm$core$List$isEmpty(nodes) ? $elm$core$Maybe$Just(result) : (($elm$core$List$length(nodes) === 1) ? A2(
					$elm$core$Maybe$map,
					function (_v0) {
						var node = _v0.b;
						return A3(
							$elm$core$Dict$insert,
							key,
							$author$project$Algo$Matcher$InternalValue_(
								A2($author$project$Algo$Matcher$toInternalReplacement_, $elm$core$Dict$empty, node)),
							result);
					},
					$elm$core$List$head(nodes)) : function (_v3) {
					var a = _v3.a;
					var b = _v3.b;
					var processChildren = $elm$core$List$map(
						function (_v4) {
							var _v5 = _v4.b;
							var n = _v5.b;
							return A2($author$project$Algo$Matcher$toInternalReplacement_, $elm$core$Dict$empty, n);
						});
					return $elm$core$Maybe$Just(
						A3(
							$elm$core$Dict$insert,
							key,
							$author$project$Algo$Matcher$MultiValue_(
								{
									e: 0,
									j: associative,
									g: commutative,
									bG: op,
									bM: processChildren(b),
									bN: processChildren(a)
								}),
							result));
				}(
					A2(
						$elm$core$List$partition,
						function (_v1) {
							var index = _v1.a;
							var _v2 = _v1.b;
							var preIndex = _v2.a;
							return _Utils_eq(index, preIndex);
						},
						A2(
							$elm$core$List$indexedMap,
							$elm$core$Tuple$pair,
							A2($elm$core$List$sortBy, $elm$core$Tuple$first, nodes)))));
			});
	});
var $author$project$Algo$Matcher$priorityList_ = function (set) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$partition(
			function (n) {
				return A2(
					$elm$core$Set$member,
					$author$project$Algo$Matcher$getID(
						$author$project$Algo$Math$getState(n)),
					set);
			}),
		function (_v0) {
			var a = _v0.a;
			var b = _v0.b;
			return _Utils_ap(a, b);
		});
};
var $author$project$Algo$Backtrack$List_ = function (a) {
	return {$: 2, a: a};
};
var $author$project$Helper$maybeList = F2(
	function (process, start) {
		return A2(
			$elm$core$List$foldl,
			F2(
				function (elem, res) {
					return A2(
						$elm$core$Maybe$andThen,
						process(elem),
						res);
				}),
			$elm$core$Maybe$Just(start));
	});
var $author$project$Algo$Backtrack$removeSelectedChildren_ = F2(
	function (p, c) {
		switch (p.$) {
			case 0:
				return $elm$core$Maybe$Just(_List_Nil);
			case 1:
				var index = p.a;
				return ((index < 0) || (_Utils_cmp(
					index,
					$elm$core$List$length(c)) > -1)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
					_Utils_ap(
						A2($elm$core$List$take, index, c),
						A2($elm$core$List$drop, index + 1, c)));
			default:
				var list = p.a;
				return A3(
					$author$project$Helper$maybeList,
					F2(
						function (tok, children) {
							return A2(
								$elm$core$Maybe$andThen,
								function (_v1) {
									var childP = _v1.b;
									return A2($author$project$Algo$Backtrack$removeSelectedChildren_, childP, children);
								},
								tok.p);
						}),
					c,
					list);
		}
	});
var $author$project$Algo$Backtrack$processNext_ = F4(
	function (_eval, nextEvals, choices, inputTok) {
		return A2(
			$elm$core$Maybe$andThen,
			function (childTok) {
				var _v9 = childTok.p;
				if (!_v9.$) {
					var _v10 = _v9.a;
					var res = _v10.a;
					var p = _v10.b;
					return A2(
						$elm$core$Maybe$andThen,
						function (newChoices) {
							var _v11 = A3(
								$author$project$Algo$Backtrack$run,
								nextEvals,
								newChoices,
								{aS: res, p: $elm$core$Maybe$Nothing});
							if (_v11.$ === 1) {
								return A4($author$project$Algo$Backtrack$processNext_, _eval, nextEvals, choices, childTok);
							} else {
								var finalTok = _v11.a;
								var _v12 = finalTok.p;
								if ((!_v12.$) && (_v12.a.b.$ === 2)) {
									var _v13 = _v12.a;
									var finalRes = _v13.a;
									var list = _v13.b.a;
									return $elm$core$Maybe$Just(
										_Utils_update(
											inputTok,
											{
												p: $elm$core$Maybe$Just(
													_Utils_Tuple2(
														finalRes,
														$author$project$Algo$Backtrack$List_(
															A2($elm$core$List$cons, childTok, list))))
											}));
								} else {
									return $elm$core$Maybe$Nothing;
								}
							}
						},
						A2($author$project$Algo$Backtrack$removeSelectedChildren_, p, choices));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2(_eval, choices, inputTok));
	});
var $author$project$Algo$Backtrack$run = F3(
	function (evals, choices, tok) {
		if (!evals.b) {
			var _v1 = tok.p;
			if (_v1.$ === 1) {
				return $elm$core$Maybe$Just(
					_Utils_update(
						tok,
						{
							p: $elm$core$Maybe$Just(
								_Utils_Tuple2(
									tok.aS,
									$author$project$Algo$Backtrack$List_(_List_Nil)))
						}));
			} else {
				return $elm$core$Maybe$Nothing;
			}
		} else {
			var _eval = evals.a;
			var nextEvals = evals.b;
			var _v2 = tok.p;
			if (_v2.$ === 1) {
				return A4($author$project$Algo$Backtrack$processNext_, _eval, nextEvals, choices, tok);
			} else {
				if ((_v2.a.b.$ === 2) && _v2.a.b.a.b) {
					var _v3 = _v2.a;
					var oldRes = _v3.a;
					var _v4 = _v3.b.a;
					var currentTok = _v4.a;
					var otherToks = _v4.b;
					return A2(
						$elm$core$Maybe$andThen,
						function (newChoices) {
							var _v6 = A3(
								$author$project$Algo$Backtrack$run,
								nextEvals,
								newChoices,
								_Utils_update(
									tok,
									{
										p: $elm$core$Maybe$Just(
											_Utils_Tuple2(
												oldRes,
												$author$project$Algo$Backtrack$List_(otherToks)))
									}));
							if (_v6.$ === 1) {
								return A4($author$project$Algo$Backtrack$processNext_, _eval, nextEvals, choices, currentTok);
							} else {
								var newNext = _v6.a;
								var _v7 = newNext.p;
								if ((!_v7.$) && (_v7.a.b.$ === 2)) {
									var _v8 = _v7.a;
									var finalRes = _v8.a;
									var finalList = _v8.b.a;
									return $elm$core$Maybe$Just(
										_Utils_update(
											tok,
											{
												p: $elm$core$Maybe$Just(
													_Utils_Tuple2(
														finalRes,
														$author$project$Algo$Backtrack$List_(
															A2($elm$core$List$cons, currentTok, finalList))))
											}));
								} else {
									return $elm$core$Maybe$Nothing;
								}
							}
						},
						A2(
							$elm$core$Maybe$andThen,
							function (_v5) {
								var childP = _v5.b;
								return A2($author$project$Algo$Backtrack$removeSelectedChildren_, childP, choices);
							},
							currentTok.p));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $author$project$Algo$Backtrack$Continuation = F2(
	function (initial, progress) {
		return {aS: initial, p: progress};
	});
var $author$project$Algo$Backtrack$init = function (initial) {
	return A2($author$project$Algo$Backtrack$Continuation, initial, $elm$core$Maybe$Nothing);
};
var $author$project$Algo$Backtrack$checkOption_ = F5(
	function (func, slot, index, choices, tok) {
		checkOption_:
		while (true) {
			if (!choices.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var choice = choices.a;
				var other = choices.b;
				var testChoice = function (token) {
					var _v4 = A3(func, slot, choice, token);
					if (_v4.$ === 1) {
						return A5(
							$author$project$Algo$Backtrack$checkOption_,
							func,
							slot,
							index + 1,
							other,
							_Utils_update(
								tok,
								{p: $elm$core$Maybe$Nothing}));
					} else {
						var child = _v4.a;
						return A2(
							$elm$core$Maybe$map,
							function (res) {
								return _Utils_update(
									tok,
									{
										p: $elm$core$Maybe$Just(
											_Utils_Tuple2(
												res,
												A2($author$project$Algo$Backtrack$Choice_, index, child)))
									});
							},
							$author$project$Algo$Backtrack$getState(child));
					}
				};
				var _v1 = tok.p;
				if (_v1.$ === 1) {
					return testChoice(tok);
				} else {
					if (_v1.a.b.$ === 1) {
						var _v2 = _v1.a;
						var old = _v2.a;
						var _v3 = _v2.b;
						var num = _v3.a;
						var childTok = _v3.b;
						if ((num < 0) || (_Utils_cmp(
							num,
							$elm$core$List$length(choices)) > -1)) {
							return $elm$core$Maybe$Nothing;
						} else {
							if (!num) {
								return testChoice(childTok);
							} else {
								var $temp$func = func,
									$temp$slot = slot,
									$temp$index = num,
									$temp$choices = A2($elm$core$List$drop, num, choices),
									$temp$tok = _Utils_update(
									tok,
									{
										p: $elm$core$Maybe$Just(
											_Utils_Tuple2(
												old,
												A2($author$project$Algo$Backtrack$Choice_, 0, childTok)))
									});
								func = $temp$func;
								slot = $temp$slot;
								index = $temp$index;
								choices = $temp$choices;
								tok = $temp$tok;
								continue checkOption_;
							}
						}
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}
			}
		}
	});
var $author$project$Algo$Backtrack$unorderedStack = function (func) {
	return $elm$core$List$map(
		function (s) {
			return A3($author$project$Algo$Backtrack$checkOption_, func, s, 0);
		});
};
var $author$project$Algo$Math$equal = F3(
	function (check, left, right) {
		var _v0 = _Utils_Tuple2(left, right);
		_v0$6:
		while (true) {
			switch (_v0.a.$) {
				case 0:
					if (!_v0.b.$) {
						var l = _v0.a.a;
						var r = _v0.b.a;
						return A2(check, l.b, r.b) && _Utils_eq(l.x, r.x);
					} else {
						break _v0$6;
					}
				case 1:
					if (_v0.b.$ === 1) {
						var l = _v0.a.a;
						var r = _v0.b.a;
						return A2(check, l.b, r.b) && _Utils_eq(l.a, r.a);
					} else {
						break _v0$6;
					}
				case 2:
					if (_v0.b.$ === 2) {
						var l = _v0.a.a;
						var r = _v0.b.a;
						return A2(check, l.b, r.b) && (_Utils_eq(l.a, r.a) && A3($author$project$Algo$Math$equal, check, l.N, r.N));
					} else {
						break _v0$6;
					}
				case 3:
					if (_v0.b.$ === 3) {
						var l = _v0.a.a;
						var r = _v0.b.a;
						return A2(check, l.b, r.b) && (_Utils_eq(l.a, r.a) && (_Utils_eq(l.j, r.j) && (_Utils_eq(l.g, r.g) && (_Utils_eq(
							$elm$core$List$length(l.af),
							$elm$core$List$length(r.af)) && A2(
							$elm$core$Maybe$withDefault,
							false,
							A2(
								$elm$core$Maybe$andThen,
								$author$project$Algo$Backtrack$getState,
								A3(
									$author$project$Algo$Backtrack$run,
									A2(
										$author$project$Algo$Backtrack$unorderedStack,
										F3(
											function (lhs, rhs, result) {
												return A3($author$project$Algo$Math$equal, check, lhs, rhs) ? A2($author$project$Algo$Backtrack$return, $elm$core$Maybe$Just, result) : $author$project$Algo$Backtrack$fail;
											}),
										l.af),
									r.af,
									$author$project$Algo$Backtrack$init(true))))))));
					} else {
						break _v0$6;
					}
				case 4:
					if (_v0.b.$ === 4) {
						var l = _v0.a.a;
						var r = _v0.b.a;
						return A2(check, l.b, r.b) && (_Utils_eq(l.a, r.a) && (_Utils_eq(
							$elm$core$List$length(l.af),
							$elm$core$List$length(r.af)) && A3(
							$elm$core$List$foldl,
							$elm$core$Basics$and,
							true,
							A3(
								$elm$core$List$map2,
								$author$project$Algo$Math$equal(check),
								l.af,
								r.af))));
					} else {
						break _v0$6;
					}
				default:
					if (_v0.b.$ === 5) {
						var l = _v0.a.a;
						var r = _v0.b.a;
						return A2(check, l.b, r.b) && (_Utils_eq(l.a, r.a) && (_Utils_eq(
							$elm$core$List$length(l.af),
							$elm$core$List$length(r.af)) && A3(
							$elm$core$List$foldl,
							$elm$core$Basics$and,
							true,
							A3(
								$elm$core$List$map2,
								$author$project$Algo$Math$equal(check),
								l.af,
								r.af))));
					} else {
						break _v0$6;
					}
			}
		}
		return false;
	});
var $author$project$Algo$Matcher$subtreeEqual_ = $author$project$Algo$Math$equal(
	F2(
		function (l, r) {
			var _v0 = _Utils_Tuple2(l, r);
			_v0$2:
			while (true) {
				if (_v0.a.b.$ === 1) {
					if (_v0.b.b.$ === 1) {
						var _v1 = _v0.a;
						var _v2 = _v1.b;
						var _v3 = _v0.b;
						var _v4 = _v3.b;
						return true;
					} else {
						break _v0$2;
					}
				} else {
					if (!_v0.b.b.$) {
						var _v5 = _v0.a;
						var a = _v5.b.a;
						var _v6 = _v0.b;
						var b = _v6.b.a;
						return _Utils_eq(a, b);
					} else {
						break _v0$2;
					}
				}
			}
			return false;
		}));
var $author$project$Algo$Matcher$extractPattern_ = F4(
	function (priority, from, root, token) {
		extractPattern_:
		while (true) {
			switch (from.$) {
				case 1:
					var s = from.a;
					if (!root.$) {
						var n = root.a;
						return _Utils_eq(n.x, s.x) ? A2($author$project$Algo$Backtrack$return, $elm$core$Maybe$Just, token) : $author$project$Algo$Backtrack$fail;
					} else {
						return $author$project$Algo$Backtrack$fail;
					}
				case 0:
					var s = from.a;
					if (root.$ === 5) {
						return $author$project$Algo$Backtrack$fail;
					} else {
						return A2(
							$author$project$Algo$Backtrack$return,
							function (result) {
								return function (varDict) {
									var newSubtree = A2($author$project$Algo$Matcher$toInternalReplacement_, varDict, root);
									var _v3 = A2($elm$core$Dict$get, s.a, result);
									if (!_v3.$) {
										if (_v3.a.$ === 1) {
											var existing = _v3.a.a;
											return A2($author$project$Algo$Matcher$subtreeEqual_, newSubtree, existing) ? $elm$core$Maybe$Just(result) : $elm$core$Maybe$Nothing;
										} else {
											return $elm$core$Maybe$Nothing;
										}
									} else {
										return $elm$core$Maybe$Just(
											A3(
												$elm$core$Dict$insert,
												s.a,
												$author$project$Algo$Matcher$InternalValue_(newSubtree),
												result));
									}
								}(
									$elm$core$Dict$fromList(
										A2(
											$elm$core$List$indexedMap,
											F2(
												function (a, b) {
													return _Utils_Tuple2(b, a);
												}),
											s.e)));
							},
							token);
					}
				case 2:
					var s = from.a;
					if ($elm$core$List$isEmpty(s.e)) {
						if (root.$ === 1) {
							var n = root.a;
							return _Utils_eq(n.a, s.a) ? A2($author$project$Algo$Backtrack$return, $elm$core$Maybe$Just, token) : $author$project$Algo$Backtrack$fail;
						} else {
							return $author$project$Algo$Backtrack$fail;
						}
					} else {
						switch (root.$) {
							case 4:
								var n = root.a;
								return ((!_Utils_eq(n.a, s.a)) || (!_Utils_eq(
									$elm$core$List$length(s.e),
									$elm$core$List$length(n.af)))) ? $author$project$Algo$Backtrack$fail : A3(
									$author$project$Algo$Backtrack$run,
									A2(
										$author$project$Algo$Backtrack$orderedStack,
										$author$project$Algo$Matcher$extractPattern_(priority),
										s.e),
									n.af,
									token);
							case 2:
								var n = root.a;
								if (!_Utils_eq(n.a, s.a)) {
									return $author$project$Algo$Backtrack$fail;
								} else {
									var _v6 = s.e;
									if (_v6.b && (!_v6.b.b)) {
										var arg = _v6.a;
										var $temp$priority = priority,
											$temp$from = arg,
											$temp$root = n.N,
											$temp$token = token;
										priority = $temp$priority;
										from = $temp$from;
										root = $temp$root;
										token = $temp$token;
										continue extractPattern_;
									} else {
										return $author$project$Algo$Backtrack$fail;
									}
								}
							default:
								return $author$project$Algo$Backtrack$fail;
						}
					}
				case 3:
					var s = from.a;
					if (root.$ === 3) {
						var n = root.a;
						if (!_Utils_eq(s.a, n.a)) {
							return $author$project$Algo$Backtrack$fail;
						} else {
							var _v8 = s.ay;
							if (_v8.$ === 1) {
								var priorityList = A2($author$project$Algo$Matcher$priorityList_, priority, n.af);
								return (!_Utils_eq(
									$elm$core$List$length(s.e),
									$elm$core$List$length(priorityList))) ? $author$project$Algo$Backtrack$fail : A3(
									$author$project$Algo$Backtrack$run,
									A2(
										$author$project$Algo$Backtrack$unorderedStack,
										$author$project$Algo$Matcher$extractPattern_(priority),
										s.e),
									priorityList,
									token);
							} else {
								var o = _v8.a;
								return function (_v10) {
									var a = _v10.a;
									var b = _v10.b;
									return A3(
										$author$project$Algo$Backtrack$run,
										_Utils_ap(
											A2(
												$author$project$Algo$Backtrack$unorderedStack,
												F2(
													function (slot, _v11) {
														var c = _v11.b;
														return A3($author$project$Algo$Matcher$extractPattern_, priority, slot, c);
													}),
												s.e),
											_List_fromArray(
												[
													A4($author$project$Algo$Matcher$otherMatchEvaluator_, s.a, n.j, n.g, o)
												])),
										_Utils_ap(a, b),
										token);
								}(
									A2(
										$elm$core$List$partition,
										function (_v9) {
											var childN = _v9.b;
											return A2(
												$elm$core$Set$member,
												$author$project$Algo$Matcher$getID(
													$author$project$Algo$Math$getState(childN)),
												priority);
										},
										A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, n.af)));
							}
						}
					} else {
						return $author$project$Algo$Backtrack$fail;
					}
				default:
					var s = from.a;
					if (root.$ === 5) {
						var n = root.a;
						if ((!_Utils_eq(s.a, n.a)) || (!_Utils_eq(
							$elm$core$List$length(s.e),
							$elm$core$List$length(n.af)))) {
							return $author$project$Algo$Backtrack$fail;
						} else {
							var priorityList = A2($author$project$Algo$Matcher$priorityList_, priority, n.af);
							return s.g ? A3(
								$author$project$Algo$Backtrack$run,
								A2(
									$author$project$Algo$Backtrack$unorderedStack,
									$author$project$Algo$Matcher$extractPattern_(priority),
									s.e),
								priorityList,
								token) : A3(
								$author$project$Algo$Backtrack$run,
								A2(
									$author$project$Algo$Backtrack$orderedStack,
									$author$project$Algo$Matcher$extractPattern_(priority),
									s.e),
								priorityList,
								token);
						}
					} else {
						return $author$project$Algo$Backtrack$fail;
					}
			}
		}
	});
var $author$project$Algo$Matcher$matchSubtree = F3(
	function (priority, matcher, root) {
		return A2(
			$elm$core$Maybe$andThen,
			$author$project$Algo$Backtrack$getState,
			A4(
				$author$project$Algo$Matcher$extractPattern_,
				priority,
				matcher,
				root,
				$author$project$Algo$Backtrack$init($elm$core$Dict$empty)));
	});
var $author$project$UI$ActionView$matchRule_ = F2(
	function (selected, rule) {
		return _Utils_Tuple2(
			rule.a$,
			function () {
				if (selected.$ === 1) {
					return $author$project$UI$ActionView$DisplayOnly;
				} else {
					var n = selected.a;
					var matches = A2(
						$elm$core$List$filterMap,
						function (m) {
							return A2(
								$elm$core$Maybe$map,
								function (result) {
									return {a3: result, a: m.bj.a, cs: m.bj.ar};
								},
								A3($author$project$Algo$Matcher$matchSubtree, n.R, m.a3.ar, n.aN));
						},
						rule.aT);
					return $elm$core$List$isEmpty(matches) ? $author$project$UI$ActionView$Disallowed : $author$project$UI$ActionView$Allowed(
						$author$project$Components$Rules$Apply(
							{aT: matches, bb: rule.bb, a$: rule.a$}));
				}
			}());
	});
var $author$project$Algo$Matcher$selectedSubtree = F2(
	function (ids, eq) {
		var _v0 = A2($author$project$Algo$Matcher$affectedSubtree_, ids, eq.m.aJ);
		if (_v0.$ === 1) {
			return $elm$core$Result$Err('Nodes not found');
		} else {
			var _v1 = _v0.a;
			var id = _v1.a;
			var nodes = _v1.b;
			return A2(
				$elm$core$Result$map,
				function (_v2) {
					var root = _v2.a;
					return _Utils_Tuple3(id, nodes, root);
				},
				A3(
					$author$project$Algo$Matcher$processSubtree_,
					A2($author$project$Algo$Matcher$searchPath_, eq.m.aJ, id),
					function (subEq) {
						return $elm$core$Result$Ok(
							_Utils_Tuple2(subEq.ar, subEq));
					},
					eq));
		}
	});
var $author$project$UI$ActionView$selectedNode_ = function (model) {
	return A2(
		$elm$core$Maybe$andThen,
		function (_v0) {
			var eqNum = _v0.a;
			var ids = _v0.b;
			return A2(
				$elm$core$Maybe$map,
				function (_v1) {
					var root = _v1.a;
					var nodes = _v1.b;
					var tree = _v1.c;
					return {ai: eqNum, R: nodes, ar: root, aN: tree};
				},
				A2(
					$elm$core$Maybe$andThen,
					A2(
						$elm$core$Basics$composeR,
						$author$project$Algo$History$current,
						A2(
							$elm$core$Basics$composeR,
							$author$project$Algo$Matcher$selectedSubtree(ids),
							$elm$core$Result$toMaybe)),
					A2($elm$core$Dict$get, eqNum, model.f)));
		},
		model.n);
};
var $author$project$UI$ActionView$view = F3(
	function (converter, rModel, dModel) {
		var selectedNode = $author$project$UI$ActionView$selectedNode_(dModel);
		return A2(
			$elm$html$Html$map,
			converter,
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('actions')
					]),
				_List_fromArray(
					[
						A3(
						$elm$html$Html$Keyed$node,
						'ul',
						_List_Nil,
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'Core',
								A2(
									$author$project$UI$ActionView$displayTopic_,
									'Core',
									$author$project$UI$ActionView$coreToList_(
										A2($author$project$UI$ActionView$coreTopic_, rModel, selectedNode)))),
							A2(
								$elm$core$List$map,
								function (topic) {
									return _Utils_Tuple2(
										topic.a,
										A2(
											$author$project$UI$ActionView$displayTopic_,
											topic.a,
											A2(
												$elm$core$List$map,
												$author$project$UI$ActionView$matchRule_(selectedNode),
												topic.bd)));
								},
								$author$project$Components$Rules$loadedTopics(rModel))))
					])));
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$mask = $elm$svg$Svg$trustedNode('mask');
var $elm$svg$Svg$Attributes$mask = _VirtualDom_attribute('mask');
var $author$project$UI$Icon$cancel = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
			attr),
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
var $author$project$UI$Dialog$FunctionValue = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$UI$Dialog$IntValue = function (a) {
	return {$: 1, a: a};
};
var $author$project$UI$Dialog$TextValue = function (a) {
	return {$: 0, a: a};
};
var $author$project$UI$Dialog$decoder_ = function (model) {
	var valueDecoder = function (name) {
		return A2(
			$elm$json$Json$Decode$field,
			name,
			A2($elm$json$Json$Decode$field, 'value', $elm$json$Json$Decode$string));
	};
	return A2(
		$elm$json$Json$Decode$map,
		model.a_,
		A3(
			$elm$core$List$foldl,
			F2(
				function (section, d) {
					return A3(
						$elm$core$List$foldl,
						F2(
							function (lines, d1) {
								return A3(
									$elm$core$List$foldl,
									F2(
										function (input, dict) {
											switch (input.$) {
												case 0:
													var s = input.a;
													return A3(
														$elm$json$Json$Decode$map2,
														function (val) {
															return A2(
																$elm$core$Dict$insert,
																s.ci,
																$author$project$UI$Dialog$TextValue(val));
														},
														valueDecoder(s.ci),
														dict);
												case 3:
													var s = input.a;
													return A2(
														$elm$json$Json$Decode$andThen,
														function (_v1) {
															var val = _v1.a;
															var map = _v1.b;
															var _v2 = $elm$core$String$toInt(val);
															if (_v2.$ === 1) {
																return $elm$json$Json$Decode$fail('Selection was not a number');
															} else {
																var num = _v2.a;
																return $elm$json$Json$Decode$succeed(
																	A3(
																		$elm$core$Dict$insert,
																		s.a,
																		$author$project$UI$Dialog$IntValue(num),
																		map));
															}
														},
														A3(
															$elm$json$Json$Decode$map2,
															$elm$core$Tuple$pair,
															valueDecoder(s.a),
															dict));
												case 4:
													var s = input.a;
													return A4(
														$elm$json$Json$Decode$map3,
														F3(
															function (orig, equal, list) {
																return A3(
																	$elm$core$Dict$insert,
																	s.a,
																	A2($author$project$UI$Dialog$FunctionValue, list, equal),
																	orig);
															}),
														dict,
														valueDecoder(s.a),
														A3(
															$elm$core$List$foldl,
															function (name) {
																return A2(
																	$elm$json$Json$Decode$map2,
																	$elm$core$List$cons,
																	valueDecoder(name));
															},
															$elm$json$Json$Decode$succeed(_List_Nil),
															A2(
																$elm$core$List$map,
																function (num) {
																	return _Utils_ap(
																		s.a,
																		$elm$core$String$fromInt(num));
																},
																A2($elm$core$List$range, 1, s.e))));
												default:
													return dict;
											}
										}),
									d1,
									lines);
							}),
						d,
						section.aw);
				}),
			$elm$json$Json$Decode$succeed($elm$core$Dict$empty),
			model.aW));
};
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$UI$Dialog$listView_ = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$map(
		function (input) {
			switch (input.$) {
				case 0:
					var t = input.a;
					return A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for(t.ci)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('text'),
										$elm$html$Html$Attributes$name(t.ci),
										$elm$html$Html$Attributes$id(
										$author$project$UI$Dialog$fieldID(t.ci))
									]),
								_List_Nil)
							]));
				case 1:
					var m = input.a;
					return A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('button'),
								$author$project$UI$HtmlEvent$onClick(m.cg),
								$author$project$UI$Icon$class('clickable')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(m.aL)
							]));
				case 2:
					var i = input.a;
					return $elm$html$Html$text(i.aL);
				case 3:
					var r = input.a;
					return A2(
						$elm$html$Html$span,
						_List_Nil,
						A2(
							$elm$core$List$concatMap,
							function (_v1) {
								var num = _v1.a;
								var t = _v1.b;
								var n = $elm$core$String$fromInt(num);
								var id = $author$project$UI$Dialog$fieldID(
									_Utils_ap(r.a, n));
								return _List_fromArray(
									[
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$type_('radio'),
												$elm$html$Html$Attributes$name(r.a),
												$elm$html$Html$Attributes$id(id),
												$elm$html$Html$Attributes$value(n)
											]),
										_List_Nil),
										A2(
										$elm$html$Html$label,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$for(id)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(t)
											]))
									]);
							},
							$elm$core$Dict$toList(r.bH)));
				default:
					var f = input.a;
					return A2(
						$elm$html$Html$span,
						_List_Nil,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$text(f.a + '('),
							_Utils_ap(
								A2(
									$elm$core$List$intersperse,
									$elm$html$Html$text(','),
									A2(
										$elm$core$List$map,
										function (index) {
											var n = _Utils_ap(
												f.a,
												$elm$core$String$fromInt(index));
											return A2(
												$elm$html$Html$label,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$for(n)
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$input,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$type_('text'),
																$elm$html$Html$Attributes$name(n),
																$elm$html$Html$Attributes$id(
																$author$project$UI$Dialog$fieldID(n))
															]),
														_List_Nil)
													]));
										},
										A2($elm$core$List$range, 1, f.e))),
								_List_fromArray(
									[
										$elm$html$Html$text(') = '),
										A2(
										$elm$html$Html$label,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$for(f.a)
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$input,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('text'),
														$elm$html$Html$Attributes$name(f.a),
														$elm$html$Html$Attributes$id(
														$author$project$UI$Dialog$fieldID(f.a))
													]),
												_List_Nil)
											]))
									]))));
			}
		}),
	$elm$html$Html$span(_List_Nil));
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $author$project$UI$HtmlEvent$onSubmitForm = function (target) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'submit',
		A2(
			$elm$json$Json$Decode$map,
			function (input) {
				return _Utils_Tuple2(input, true);
			},
			A2($elm$json$Json$Decode$field, 'target', target)));
};
var $elm$html$Html$section = _VirtualDom_node('section');
var $author$project$UI$Icon$tick = function (attr) {
	return A2(
		$elm$svg$Svg$svg,
		A2(
			$elm$core$List$cons,
			$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
			attr),
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
var $author$project$UI$Dialog$view = function (model) {
	return A3(
		$elm$html$Html$node,
		'dialog',
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'open', 'true')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h1,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(model.a$)
					])),
				A2(
				$elm$html$Html$form,
				_List_fromArray(
					[
						$author$project$UI$HtmlEvent$onSubmitForm(
						$author$project$UI$Dialog$decoder_(model)),
						A2($elm$html$Html$Attributes$attribute, 'method', 'dialog')
					]),
				_Utils_ap(
					A2(
						$elm$core$List$map,
						function (section) {
							return A2(
								$elm$html$Html$section,
								_List_Nil,
								A2(
									$elm$core$List$cons,
									A2(
										$elm$html$Html$h2,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(section.aB)
											])),
									A2($elm$core$List$map, $author$project$UI$Dialog$listView_, section.aw)));
						},
						model.aW),
					_List_fromArray(
						[
							$author$project$UI$Icon$cancel(
							_List_fromArray(
								[
									$author$project$UI$Icon$class('clickable'),
									$author$project$UI$Icon$class('cancelable'),
									$author$project$UI$HtmlEvent$onClick(model.aQ)
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('submit'),
									$elm$html$Html$Attributes$class('noDefault')
								]),
							_List_fromArray(
								[
									$author$project$UI$Icon$tick(
									_List_fromArray(
										[
											$author$project$UI$Icon$class('clickable'),
											$author$project$UI$Icon$class('submitable')
										]))
								]))
						])))
			]));
};
var $author$project$UI$HistoryView$DisplayEvent = function (a) {
	return {$: 0, a: a};
};
var $author$project$UI$HistoryView$DraggableEvent = function (a) {
	return {$: 1, a: a};
};
var $author$project$Components$Display$HistoryEvent = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Algo$History$SelectPast = $elm$core$Basics$identity;
var $author$project$UI$Draggable$Bottom = 3;
var $author$project$UI$Draggable$BottomLeft = 6;
var $author$project$UI$Draggable$BottomRight = 7;
var $author$project$UI$Draggable$DragStart = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $author$project$UI$Draggable$Left = 0;
var $author$project$UI$Draggable$Right = 1;
var $author$project$UI$Draggable$Top = 2;
var $author$project$UI$Draggable$TopLeft = 4;
var $author$project$UI$Draggable$TopRight = 5;
var $author$project$UI$Draggable$Drag = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$UI$Draggable$DragEnd = function (a) {
	return {$: 2, a: a};
};
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 3, a: a};
};
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $author$project$UI$HtmlEvent$onPointerMove = F3(
	function (converter, move, cancel) {
		var positionDecoder = A3(
			$elm$json$Json$Decode$map2,
			$elm$core$Tuple$pair,
			A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float));
		var pointerId = A2($elm$json$Json$Decode$field, 'pointerId', $elm$json$Json$Decode$value);
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$Events$custom,
				'pointermove',
				A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (pid, input) {
							return {
								aU: converter(
									A2(move, pid, input)),
								aV: true,
								aZ: true
							};
						}),
					pointerId,
					positionDecoder)),
				A2(
				$elm$html$Html$Events$custom,
				'pointerup',
				A2(
					$elm$json$Json$Decode$map,
					function (pid) {
						return {
							aU: converter(
								cancel(pid)),
							aV: true,
							aZ: true
						};
					},
					pointerId)),
				A2(
				$elm$html$Html$Events$custom,
				'pointercancel',
				A2(
					$elm$json$Json$Decode$map,
					function (pid) {
						return {
							aU: converter(
								cancel(pid)),
							aV: true,
							aZ: true
						};
					},
					pointerId))
			]);
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$UI$Draggable$divAttrs_ = F2(
	function (converter, model) {
		return _Utils_ap(
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$core$String$fromFloat(model.q.w) + 'dvw'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$core$String$fromFloat(model.q.E) + 'dvh'),
					A2(
					$elm$html$Html$Attributes$style,
					'width',
					$elm$core$String$fromFloat(model.q.A - model.q.w) + 'dvw'),
					A2(
					$elm$html$Html$Attributes$style,
					'height',
					$elm$core$String$fromFloat(model.q.B - model.q.E) + 'dvh'),
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2($elm$html$Html$Attributes$style, 'padding', '1rem 0.2rem 0.2rem 0.2rem'),
					$elm$html$Html$Attributes$id(model.ci)
				]),
			A3($author$project$UI$HtmlEvent$onPointerMove, converter, $author$project$UI$Draggable$Drag, $author$project$UI$Draggable$DragEnd));
	});
var $author$project$UI$HtmlEvent$onPointerCapture = F2(
	function (converter, activate) {
		return A2(
			$elm$html$Html$Events$custom,
			'pointerdown',
			A3(
				$elm$json$Json$Decode$map2,
				F2(
					function (pid, input) {
						return {
							aU: converter(
								A2(activate, pid, input)),
							aV: true,
							aZ: true
						};
					}),
				A2($elm$json$Json$Decode$field, 'pointerId', $elm$json$Json$Decode$value),
				A3(
					$elm$json$Json$Decode$map2,
					$elm$core$Tuple$pair,
					A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
					A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float))));
	});
var $author$project$UI$Draggable$div = F4(
	function (converter, model, attrs, children) {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				attrs,
				A2($author$project$UI$Draggable$divAttrs_, converter, model)),
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(2)),
								A2($elm$html$Html$Attributes$style, 'left', '0'),
								A2($elm$html$Html$Attributes$style, 'top', '0'),
								A2($elm$html$Html$Attributes$style, 'width', '100%'),
								A2($elm$html$Html$Attributes$style, 'height', '1rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'move')
							]),
						_List_fromArray(
							[
								$author$project$UI$Icon$menu(
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'height', '1rem')
									]))
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(0)),
								A2($elm$html$Html$Attributes$style, 'left', '0'),
								A2($elm$html$Html$Attributes$style, 'top', '0'),
								A2($elm$html$Html$Attributes$style, 'height', '100%'),
								A2($elm$html$Html$Attributes$style, 'width', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'ew-resize')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(3)),
								A2($elm$html$Html$Attributes$style, 'left', '0'),
								A2($elm$html$Html$Attributes$style, 'bottom', '0'),
								A2($elm$html$Html$Attributes$style, 'width', '100%'),
								A2($elm$html$Html$Attributes$style, 'height', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'ns-resize')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(1)),
								A2($elm$html$Html$Attributes$style, 'right', '0'),
								A2($elm$html$Html$Attributes$style, 'top', '0'),
								A2($elm$html$Html$Attributes$style, 'height', '100%'),
								A2($elm$html$Html$Attributes$style, 'width', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'ew-resize')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(4)),
								A2($elm$html$Html$Attributes$style, 'left', '0'),
								A2($elm$html$Html$Attributes$style, 'top', '0'),
								A2($elm$html$Html$Attributes$style, 'height', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'width', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'nwse-resize')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(6)),
								A2($elm$html$Html$Attributes$style, 'left', '0'),
								A2($elm$html$Html$Attributes$style, 'bottom', '0'),
								A2($elm$html$Html$Attributes$style, 'height', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'width', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'nesw-resize')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(5)),
								A2($elm$html$Html$Attributes$style, 'right', '0'),
								A2($elm$html$Html$Attributes$style, 'top', '0'),
								A2($elm$html$Html$Attributes$style, 'height', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'width', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'nesw-resize')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('border'),
								A2(
								$author$project$UI$HtmlEvent$onPointerCapture,
								converter,
								$author$project$UI$Draggable$DragStart(7)),
								A2($elm$html$Html$Attributes$style, 'right', '0'),
								A2($elm$html$Html$Attributes$style, 'bottom', '0'),
								A2($elm$html$Html$Attributes$style, 'height', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'width', '0.2rem'),
								A2($elm$html$Html$Attributes$style, 'cursor', 'nwse-resize')
							]),
						_List_Nil)
					]),
				children));
	});
var $author$project$Algo$History$serialize_ = F4(
	function (converter, nodes, index, n) {
		return A3(
			converter,
			index,
			n.at,
			A2(
				$elm$core$List$filterMap,
				function (c) {
					return A2(
						$elm$core$Maybe$map,
						A3($author$project$Algo$History$serialize_, converter, nodes, c._),
						A2($elm$core$Dict$get, c._, nodes));
				},
				n.af));
	});
var $author$project$Algo$History$serialize = F2(
	function (processNode, model) {
		return A4($author$project$Algo$History$serialize_, processNode, model.R, 0, model.ar);
	});
var $author$project$UI$HistoryView$view = F3(
	function (converter, dragModel, model) {
		return A4(
			$author$project$UI$Draggable$div,
			A2($elm$core$Basics$composeR, $author$project$UI$HistoryView$DraggableEvent, converter),
			dragModel,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$core$Maybe$withDefault,
					$elm$html$Html$text('No history selected'),
					A2(
						$elm$core$Maybe$andThen,
						function (_v0) {
							var eq = _v0.a;
							return A2(
								$elm$core$Maybe$map,
								$author$project$Algo$History$serialize(
									F3(
										function (index, c, children) {
											return A2(
												$elm$html$Html$div,
												_List_Nil,
												A2(
													$elm$core$List$cons,
													A2(
														$elm$html$Html$a,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('clickable'),
																$author$project$UI$HtmlEvent$onClick(
																converter(
																	$author$project$UI$HistoryView$DisplayEvent(
																		A2($author$project$Components$Display$HistoryEvent, eq, index))))
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(
																$author$project$Algo$Math$toString(c.ar))
															])),
													children));
										})),
								A2($elm$core$Dict$get, eq, model.f));
						},
						model.n))
				]));
	});
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $author$project$UI$Menu$Click = $elm$core$Basics$identity;
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$UI$Menu$partToHtml_ = F3(
	function (converter, model, part) {
		if (part.$ === 1) {
			var children = part.a;
			return A2($elm$html$Html$li, _List_Nil, children);
		} else {
			var title = part.a;
			var children = part.b;
			var shown = A2($elm$core$Set$member, title.a, model.V);
			return A2(
				$elm$html$Html$li,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('menuSection')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h1,
						A2(
							$author$project$Helper$maybeAppend,
							A2(
								$author$project$Helper$maybeGuard,
								shown,
								$elm$html$Html$Attributes$class('shown')),
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('menuTitle'),
									$author$project$UI$HtmlEvent$onClick(
									converter(title.a)),
									$elm$html$Html$Attributes$class('clickable')
								])),
						A2(
							$author$project$Helper$maybeAppend,
							title.W,
							_List_fromArray(
								[
									$elm$html$Html$text(title.a)
								]))),
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
							A2($author$project$UI$Menu$partToHtml_, converter, model),
							children))
					]));
		}
	});
var $author$project$UI$Menu$view = F3(
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
						A2($author$project$UI$Menu$partToHtml_, converter, model),
						children))
				]));
	});
var $author$project$UI$Notification$notificationDiv_ = F3(
	function (converter, id, notification) {
		return _Utils_Tuple2(
			'notification-' + $elm$core$String$fromInt(id),
			A2(
				$elm$html$Html$map,
				converter,
				A2(
					$elm$html$Html$div,
					A2(
						$author$project$Helper$maybeAppend,
						$author$project$UI$Animation$class(notification),
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('notificationMessage'),
								$author$project$UI$HtmlEvent$onClick(
								$author$project$UI$Notification$ClearEvent(id))
							])),
					_List_fromArray(
						[
							$author$project$UI$Icon$cancel(
							_List_fromArray(
								[
									$author$project$UI$Icon$class('clickable'),
									$author$project$UI$Icon$class('cancelable')
								])),
							A2(
							$elm$html$Html$pre,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(notification.br)
								]))
						]))));
	});
var $author$project$UI$Notification$view = F3(
	function (converter, attrs, model) {
		return A3(
			$elm$html$Html$Keyed$node,
			'div',
			attrs,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var id = _v0.a;
					var val = _v0.b;
					return A3($author$project$UI$Notification$notificationDiv_, converter, id, val);
				},
				$elm$core$Dict$toList(model.M)));
	});
var $author$project$Main$view = function (core) {
	var model = core.H;
	return {
		ca: $elm$core$List$singleton(
			A3(
				$elm$html$Html$Keyed$node,
				'div',
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('body')
					]),
				A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							$elm$core$Maybe$Just(
							_Utils_Tuple2(
								'display',
								A3(
									$author$project$Components$Display$view,
									$author$project$Main$DisplayEvent,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('display')
										]),
									model.i))),
							A2(
							$author$project$Helper$maybeGuard,
							model.az,
							_Utils_Tuple2(
								'history',
								A3($author$project$UI$HistoryView$view, $author$project$Main$HistoryEvent, model.av, model.i))),
							$elm$core$Maybe$Just(
							_Utils_Tuple2(
								'actions',
								A3($author$project$UI$ActionView$view, $author$project$Main$RuleEvent, model.bd, model.i))),
							$elm$core$Maybe$Just(
							_Utils_Tuple2(
								'inputPane',
								A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('inputPane')
										]),
									_List_fromArray(
										[
											A3(
											$elm$html$Html$Keyed$node,
											'div',
											_List_fromArray(
												[
													$elm$html$Html$Attributes$id('leftPane')
												]),
											A2(
												$elm$core$List$filterMap,
												$elm$core$Basics$identity,
												_List_fromArray(
													[
														A2(
														$author$project$Helper$maybeGuard,
														model.U,
														_Utils_Tuple2(
															'helpText',
															A2(
																$elm$html$Html$pre,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$id('helpText')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text($author$project$Algo$Math$notation)
																	])))),
														A2($elm$core$Maybe$map, $author$project$Main$inputDiv, model.D)
													]))),
											A2(
											$elm$html$Html$div,
											A2(
												$elm$core$List$cons,
												$elm$html$Html$Attributes$id('rightPane'),
												model.Y ? _List_Nil : _List_fromArray(
													[
														$elm$html$Html$Attributes$class('closed')
													])),
											_List_fromArray(
												[
													A3(
													$author$project$UI$Menu$view,
													$author$project$Main$MenuEvent,
													model.ax,
													_List_fromArray(
														[
															A2(
															$author$project$UI$Menu$Section,
															{W: $elm$core$Maybe$Nothing, a: 'Settings'},
															_List_fromArray(
																[
																	$author$project$UI$Menu$Content(
																	_List_fromArray(
																		[
																			A2(
																			$elm$html$Html$a,
																			_List_fromArray(
																				[
																					$author$project$UI$HtmlEvent$onClick(
																					$author$project$Main$FileSelect(1)),
																					$elm$html$Html$Attributes$class('clickable')
																				]),
																			_List_fromArray(
																				[
																					$elm$html$Html$text('Open')
																				]))
																		])),
																	$author$project$UI$Menu$Content(
																	_List_fromArray(
																		[
																			A2(
																			$elm$html$Html$a,
																			_List_fromArray(
																				[
																					$author$project$UI$HtmlEvent$onClick($author$project$Main$Save),
																					$elm$html$Html$Attributes$class('clickable')
																				]),
																			_List_fromArray(
																				[
																					$elm$html$Html$text('Save')
																				]))
																		])),
																	$author$project$UI$Menu$Content(
																	_List_fromArray(
																		[
																			A2(
																			$elm$html$Html$a,
																			_List_fromArray(
																				[
																					$author$project$UI$HtmlEvent$onClick($author$project$Main$ToggleHistory),
																					$elm$html$Html$Attributes$class('clickable')
																				]),
																			_List_fromArray(
																				[
																					$elm$html$Html$text('Show History')
																				]))
																		]))
																])),
															A2(
															$author$project$UI$Menu$Section,
															{
																W: $elm$core$Maybe$Just(
																	A2(
																		$elm$html$Html$a,
																		_List_fromArray(
																			[
																				$author$project$UI$HtmlEvent$onClick($author$project$Main$EnterCreateMode),
																				$elm$html$Html$Attributes$class('clickable')
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text('+')
																			]))),
																a: 'Equations'
															},
															A2($author$project$Components$Display$menu, $author$project$Main$DisplayEvent, model.i)),
															A2($author$project$Components$Tutorial$menu, $author$project$Main$TutorialEvent, model.aC),
															A2(
															$author$project$UI$Menu$Section,
															{
																W: $elm$core$Maybe$Just(
																	A2(
																		$elm$html$Html$a,
																		_List_fromArray(
																			[
																				$author$project$UI$HtmlEvent$onClick(
																				$author$project$Main$OpenDialog($author$project$Main$addTopicDialog_)),
																				$elm$html$Html$Attributes$class('clickable')
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text('+')
																			]))),
																a: 'Topics'
															},
															A2($author$project$Components$Rules$menuTopics, $author$project$Main$RuleEvent, model.bd))
														])),
													$author$project$UI$Icon$menu(
													A2(
														$elm$core$List$filterMap,
														$elm$core$Basics$identity,
														_List_fromArray(
															[
																$elm$core$Maybe$Just(
																$elm$html$Html$Attributes$id('menuToggle')),
																$elm$core$Maybe$Just(
																$author$project$UI$HtmlEvent$onClick($author$project$Main$ToggleMenu)),
																$elm$core$Maybe$Just(
																$author$project$UI$Icon$class('clickable')),
																A2(
																$author$project$Helper$maybeGuard,
																!model.Y,
																$author$project$UI$Icon$class('closed'))
															])))
												]))
										])))),
							A2(
							$elm$core$Maybe$map,
							function (_v0) {
								var d = _v0.a;
								return _Utils_Tuple2(
									'dialog',
									$author$project$UI$Dialog$view(d));
							},
							core.t),
							$elm$core$Maybe$Just(
							_Utils_Tuple2(
								'notification',
								A3(
									$author$project$UI$Notification$view,
									$author$project$Main$NotificationEvent,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('notification')
										]),
									model.aa)))
						])))),
		a$: 'Maths'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{ck: $author$project$Main$init, cn: $author$project$Main$EventUrlChange, co: $author$project$Main$EventUrlRequest, cy: $author$project$Main$subscriptions, cA: $author$project$Main$update, cB: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));