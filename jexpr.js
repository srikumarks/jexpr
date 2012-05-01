// Copyright (c) 2012, Srikumar K. S. (srikumarks.github.com)
//
// Code licensed for use and redistribution with warranties
// (or the lack thereof) as described in the MIT licence.
// License URL: http://www.opensource.org/licenses/MIT

// This is an attempt at developing a language using JSON objects as containers
// for the AST, similar to how list expressions serve as a representation for
// ASTs in the lisp family of languages.  The key idea exploited here is that
// browser based Javascript engines such as V8 always enumerate the keys of an
// object in the same order as they were inserted.  Though this is not required
// by ECMAScript, it is considered standard behaviour in browser environments
// and in Node.js too (since it uses V8).
//
// The overall structure of a j-expression is like this -
//
//      {operator: [args...],
//          keyword1: value1,
//          keyword2: value2,
//          ...}
//
// .. and we'll call the language "J" here for brevity.
//
// #### Relevant posts
//
// 1. [J-expressions]
// 2. [DSLs using JSON expressions]
//
// [DSLs using JSON expressions]: http://srikumarks.github.com/gyan/2012/04/14/creating-dsls-in-javascript-using-j-expressions/
// [J-expressions]: http://srikumarks.github.com/gyan/2012/04/15/j-expressions/

var J = (function () {

// We single out the first key presented in a JSON object as the name of the
// operator.
function operatorName(obj) { 
    if (obj && obj.constructor === Object) {
        for (var k in obj) { 
            return k; 
        } 
    } else {
        return undefined;
    }
};

// ## Compilation environments

// We need an environment structure to remember the scope of bindings
// established as we develop the compiler and as the compiler walks through the
// AST. We start with a simple environment definition with the ability to
// construct an environment with another one as its "parent scope".
var Env = function (base) { 
    if (base) {
        this.base = base;
        this.symbols = Object.create(base.symbols);
    } else {
        this.symbols = {};
    }
}

// We now start with the basic compiler that supports simple object types -
// numbers and booleans.  The compiled form of these is simply the JSON
// stringification so that we can insert them into the compiled code directly
// as literals.
function compile_lit(env, expr) { 
    
    if (expr === undefined || expr === null) {
        return JSON.stringify(expr); 
    }

    if (expr.constructor === Number || expr.constructor == Boolean) {
        return JSON.stringify(expr);
    }

    return undefined;
}

// Strings are a bit special. We're going to need symbols in our
// language. Since JS doesn't have a separate symbol type, we'll
// just use plain strings as symbols and worry about strings later on.
// This means we're going to need a way to lookup the compile-value of a 
// symbol in an environment first. Use a namespace prefix to avoid
// touching the builtin properties.
function lookupSymbol(env, sym) {
    return env.symbols['J_' + sym];
}

// We now add a simple function to define new things into
// a given environment.
function define(env, name, value) {
    return env.symbols['J_' + name] = value;
}

// We can now write our symbol compilation. This looks up
// the value in the environment and just returns it if found.
function compile_sym(env, sym) {
    if (sym && sym.constructor === String) {
        return lookupSymbol(env, sym);
    }

    return undefined;
}

// A new "variable" in our language will be mapped to a javascript
// variable by attaching a special prefix so that the language
// cannot escape its boundaries. We also use the environment's
// "id" number in the name so that the JS variables associated
// with different environments can be told apart.
var idRx = /^[a-zA-Z_\$][a-zA-Z0-9_\$]*$/;
function varname(env, sym) {
    if (!idRx.test(sym)) {
        throw "Bad identifier!";
    }
    return 'var$' + env.id + '$' + sym;
}

function varnames(env, syms) {
    return syms.map(function (sym) {
        return varname(env, sym);
    });
}

function newvar(env, sym) {
    return define(env, sym, varname(env, sym));
}

function newvars(env, syms) {
    return syms.map(function (sym) {
        return newvar(env, sym);
    });
};

// Oops. We haven't defined an environment's ID. Let's patch
// Env to add that.
function patch(oldEnv, change) {
    function NewEnv(base) {
        oldEnv.call(this, base);
        change.call(this, base);
    }

    NewEnv.prototype = oldEnv.prototype;
    return NewEnv;
}

var globallyUniqueEnvID = 1;
Env = patch(Env, function (base) {
    this.id = (globallyUniqueEnvID++);
});

// Lets also add some options that we can store and
// inherit over the Env chain.
Env = patch(Env, function (base) {
    this.options = (base ? Object.create(base.options) : {});
});

// We're now ready to process our first J-expression. We treat
// the first key as the symbol standing for an operator, fetch
// the function that implements the operator and just call it.
// Note that if the looked up value is a function, it is really
// a "macro" because we're writing a *compiler*. Actual value
// lookup will yield a string which we can use as a JS expression
// in the compiled result directly.
//      {operator: [arguments...], keyword1: value1, ...}
function compile_jexpr(env, jexpr) {
    if (jexpr && jexpr.constructor === Object) {
        var op = lookupSymbol(env, operatorName(jexpr));
        if (op && op.constructor === Function) {
            return op(env, jexpr); /* We have a native implementation available.
                                    * We pass the entire body of the expression
                                    * to it without evaluating anything else.
                                    */
        }

        if (op && op.constructor === String) {
            return compile_apply(env, op, jexpr); /* This is an already compiled value. So just
                                                   * treat it as a function and apply it.
                                                   */
        }

        if (!op && env.options.unsafe) {
            // Treat it as a globally available thingie.
            return compile_apply(env, operatorName(jexpr), jexpr);
        }
    }

    return undefined;
}

// We now need to build our whole compilation function so
// that we can just call it to compile any j-expression
// or primitive type.
function compile(env, expr) {
    return compile_jexpr(env, expr)
        || compile_sym(env, expr)
        || compile_lit(env, expr);
}

// To help ourselves a bit, let's define a mapping utility
// that applies a two-argument "macro-like" function to
// an array of expressions.
function map(env, fn, exprs) {
    return exprs.map(function (expr) {
        return fn(env, expr);
    });
}

// The "arguments" of an operator are provided as an array value.
// Each element is compiled in turn and the result used as the
// argument-list of the compiled javascript function.
function compile_args(env, argv) {
    if (argv && argv.constructor === Array) {
        return map(env, compile, argv).join(',');
    } else {
        // Compile it as a single expression.
        return compile(env, argv);
    }
}

// Now we are ready to write the compile_apply, which will
// apply a compiled function by symbol reference to a given
// arguments list.
function compile_apply(env, op, jexpr) {
    return op + '(' + compile_args(env, jexpr[operatorName(jexpr)]) + ')';
}


// ## Primitives
//
// Ok, so far we have not implemented any primitives. Our first one
// is going to be a mechanism for expressions to return literal JSON
// objects. This is the analog of "quote" in Scheme and we'll use
// the succinct "$" symbol as the key of a jexpr to represent quoted
// forms. We'll insert these primitives into a "primitives" environment.
var Prim = new Env;

define(Prim, '$', function (env, expr) {
    return JSON.stringify(expr.$);
});

// And now for an ultra simple "display" implementation.
// After all, how are we going to write a "hello world" program
// without this one!
define(Prim, 'display', function (env, expr) {
    return '(console.log(' + compile(env, expr.display) + '), null)';
});

// We're now ready to do a "hello world". But first some helper stuff.
// We're going to be making new environments. So let's make a helper 
// method on Env to make a new derived environment. 
function subenv(env) {
    return new Env(env);
}

// HELLO WORLD!
eval(compile(subenv(Prim), {display: {$: "Hello world!"}}));

// Let's wrap that little piece of code into a "run" method
// and insert it into the environment so that programs can
// be run within environments.

// "run" will run the expressions in a new child environment
// without affecting the target environment.
Env.prototype.run = function () {
    var env = subenv(this);
    var result;
    Array.prototype.forEach.call(arguments, function (expr) {
        result = eval(compile(env, expr));
    });
    return result;
};


// Now lets implement some more primitives!
// We'll do the useful "list" function which will take
// a bunch of arguments and produce an list (a JS array)
// out of them.
define(Prim, 'list', '(function () { return Array.prototype.slice.call(arguments, 0); })');

// ... and list length
define(Prim, 'length', '(function (x) { return x.length; })');

Prim.run({display: {length: [{list: [1,2,3]}]}});

// We'll also put in a macro for constructing tables.
// This has to be a macro because we're going to have
// to evaluate the value fields of the given object.
//
//      {table: {x: 2, y: {$: "why?"}}}
//
// should produce what you think it should.
define(Prim, 'table', function (env, expr) {
    var keys = Object.keys(expr.table);
    return '{' 
        + keys.map(function (k) {
            return JSON.stringify(k) + ':' + compile(env, expr.table[k]);
          }).join(',')
        + '}';
});

// ### Let there be lambda
//
// Now for the BIG BOY! The syntax we use for lambda is like this -
//
//      {lambda: ["arg1", "arg2", ...],
//         body: expr|[expr1, expr2, ..., exprN]}
//
// We turn that into a JS function like this -
//
//      function (arg1, arg2, ...) {
//          var keywords = this;
//          return (expr1, expr2, ... exprN);
//      }
//
//  We also make "this" available as the special symbol 'keywords'
//  with the intention of passing in optional arguments through 'this'.
define(Prim, 'lambda', function (env, expr) {
    var env2 = subenv(env);
    return '(function (' 
            + newvars(env2, expr.lambda).join(',')
            + ') {'
            + 'var ' + newvar(env2, 'keywords') + ' = this;'
            + 'return (' + compile_args(env2, expr.body) + ');})';
});

// ### Optional keyword arguments

// That was easy! ... but the lambda is unable to make
// use of optional keyword arguments yet and that would be a real waste.
// To support that, at call time, we'll pass the compiled version of
// the call expression body to the lambda as a table so that it can
// access the arguments other than the args array through the local
// "keywords" symbol. 

// First, we need to compile the entire expression as a value.
function compile_exprval(env, expr, keys) {
    return '{' 
        + keys.map(function (k) {
            return JSON.stringify(k) 
                + ':' 
                + (compile_array(env, expr[k]) || compile(env, expr[k]));
          }).join(',') 
        + '}';
}

// Now we need to patch compile_apply to check for the presence
// of keywords and if so pass it as the "this" part of a call.
var compile_apply = (function (prevCompileApply) {
    return function (env, op, jexpr) {
        var keys = Object.keys(jexpr);
        if (keys.length === 1) {
            return prevCompileApply(env, op, jexpr); // Avoid the overhead of a ".call"
        } else {
            var opname = operatorName(jexpr);
            keys.shift(); // Drop the operator.
            return op 
                + '.call(' 
                + compile_exprval(env, jexpr, keys)
                + ','
                + compile_args(env, jexpr[opname])
                + ')';
        }
    };
}(compile_apply));


// This just compiles the parts of the array and wraps it with the
// array constructor.
function compile_array(env, arr) {
    if (arr && arr.constructor === Array) {
        return '[' + compile_args(env, arr) + ']';
    }

    return undefined;
}

// And to *use* lambda, we're going to need apply.
//
//      {apply: funval, args: listval, keywords: tableval}
define(Prim, 'apply', function (env, expr) {
    return compile(env, expr.apply) 
        + '.apply('
        + (expr.keywords ? compile(env, expr.keywords) : 'null')
        + ',' 
        + compile(env, expr.args)
        + ')';
});

// Lets now try a lambda hello world.
Prim.run({apply: {lambda: ["msg"],
                    body: [
                        {display: {$: "Hello lambda world ..."}},
                        {display: "msg"},
                        {display: "keywords"}
                    ]},
            args: {list: [{$: "planet earth rocks!"}]},
            keywords: {table: {global: {$: "cooling ftw!"}}}});
        

// ### "where" clauses
//
// Now let's add something "interesting" to lambda
// - a "where" clause. The idea is that whenever we have an extra
// "where: {key1: val1, key2: val2,..}" entry in a j-expression,
// we make those keys available like local variables within the
// scope of the expression. Let's generalize this feature first.
//
// What we do is to turn {...where: {x: val1, y: val2} ...}
// as a function wrapper like -
//      
//      (function (x, y) {
//          ..expr..
//      }(val1, val2))
function whereClause(env, expr, where, macro) {
    if (!where) {
        return macro(env, expr);
    }

    var whereEnv = subenv(env);
    var whereVars = Object.keys(where);

    return '(function (' + newvars(whereEnv, whereVars) + ') {'
        + 'return (' + macro(whereEnv, expr) + ');}'
        + '(' 
        + whereVars.map(function (v) { 
            return compile(env, where[v]); 
          }).join(',') 
        + '))';
}

// Now we can add where clause support to lambda.
define(Prim, 'lambda', (function (oldLambda) {
    return function (env, expr) {
        return whereClause(env, expr, expr.where, oldLambda);
    };
}(lookupSymbol(Prim, 'lambda'))));

// ### Macros
//
// Now we up the game a bit and define the ability to
// write macros. We've already been writing macros,
// so we just need to expose that bit of functionality
// to the language itself. Macros are just lambdas that
// take the entire expression as a single argument
// and return an expression to be used instead.
//
//      {macro: "name",
//          lambda: ["expr"],
//          body: ...,
//          where: ...}
define(Prim, 'macro', function (env, expr) {
    var macrodefn = eval(lookupSymbol(env, 'lambda')(env, expr));
    define(env, expr.macro, function (env, expr) {
        return compile(env, macrodefn(expr));
    });
    return 'undefined';
});

// Woot! We have macros! ... but we can't even write a hello world
// with macros now because we don't have a proper way to construct object
// literals in our language. We can use 'list' and 'table', but yuck!
// we need a quasiquoter!
// 
//      {$_: <quoted> {_$: <unquoted>} ...}
//
// We first write a "$_" macro that will quasi quote. We make the
// unquoting mechanism generic by putting a table of unquoters for
// the quasi quoter to look for, right into the environment.
function AddUnquoters(base) {
    this.unquoters = base ? Object.create(base.unquoters) : {};
}
Env = patch(Env, AddUnquoters);
AddUnquoters.call(Prim, Prim.base);

function quasiQuote(env, expr) {
    if (expr && expr.constructor === Array) { // Array literal.
        return '[' 
            + map(env, quasiQuote, expr).join(',')
            + ']';
    }

    if (expr && expr.constructor === Object) { // Object literal ... 
        var unquoter = env.unquoters[operatorName(expr)];
        if (unquoter) { // ... but maybe an unquoter here?
            return unquoter(env, expr);
        } else {
            return '{' 
                + Object.keys(expr).map(function (k) {
                    return JSON.stringify(k) + ':' + quasiQuote(env, expr[k]);
                  }).join(',')
                + '}';
        }
    }

    return JSON.stringify(expr); // else literal.
}

// Quasiquote operator
define(Prim, '$_', function (env, expr) {
    return quasiQuote(env, expr.$_);
});

// Now we add one unquoter '_$'.
Prim.unquoters['_$'] = function (env, expr) {
    return compile(env, expr._$);
};

// Unquote splice is simple enough as well.
// Beware that it can only be used sensibly 
// when expanding arrays.
Prim.unquoters['_$$'] = function (env, expr) {
    return compile_args(env, expr._$$);
};

// Hooray! We can now do a macro hello world!
Prim.run({macro: "hello",
    lambda: ["expr"],
    body: [{$_: {display: {$: ["In macro!", {_$$: ["meow", "expr"]}]}}}],
    where: {meow: {$: "bowow"}}
},
{hello: ["macro", "world!"]});

// ## Going to town!
//
// Now we go to town and add all sorts of bells and whistles.

// ### let:in:
// First up is a variant on the "where" clause - the "let:in:".
//
//      {let: {x: blah, y: bling}, in: expr|[expr1, expr2, ...]}
define(Prim, 'let', function (env, expr) {
    return whereClause(env, expr, expr.let, function (envw, expr) {
        return compile_args(envw, expr.in);
    });
});

Prim.run({let: {msg: {$: "hello"}}, 
    in: [{display: {$_: [{_$: "msg"}, "let world"]}}]});

// ### if:then:else:
// {if: cond, then: expr1, else: expr2}
define(Prim, 'if', function (env, expr) {
    return '(' + compile(env, expr.if) 
        + '?' + compile(env, expr.then)
        + ':' + compile(env, expr.else)
        + ')';
});


// ### Generators
// Since JS doesn't support tail call elimination, we need some
// way to loop. For that, it is useful to have generators like
// in python - basically functions that you can call repeatedly 
// to get a sequence of values. Our protocol will be that the
// generator is considered to end when the function returns
// 'undefined', and we can pass in a bool value of 'true' to
// reset the generator.

// {from: ix1, to: ix2, step: dix}
// Usual defaults apply.
define(Prim, 'from', function (env, body) {
    function iterator(comp) {
        return '(function (reset) {'
            + 'if (reset) {i = from + step; return from;}\n'
            + 'var result = i;'
            + 'return (i ' + comp + ' to ? ((i += step), result) : undefined);})';
    }

    return '((function (from, to, step) {var i = from; '
        + 'if (to === undefined) {'
        +    'to = from + step * 1e16;'
        + '}\n'
        + 'return (step > 0 ?' + iterator('<') + ':' + iterator('>') + ');})('
        + compile(env, body.from) + ','
        + (body.to ? compile(env, body.to) : 'undefined') + ','
        + (body.step ? compile(env, body.step) : '1')
        + '))';
});

// {in: list, from: ix1, to: ix2, step: dix}
// Similar to from: but steps through array.
define(Prim, 'in', function (env, body) {
    function iterator(comp) {
        return '(function (reset) {'
            + 'if (reset) {i = from + step; return arr[from];}\n'
            + 'var result = arr[i];'
            + 'return (i ' + comp + ' to ? ((i += step), result) : undefined);})';
    }

    return '((function (arr, from, to, step) {var i = from; '
        + 'if (to === undefined) {'
        +    'to = (step > 0 ? arr.length : -1);'
        + '}\n'
        + 'return (step > 0 ?' + iterator('<') + ':' + iterator('>') + ');})('
        + compile(env, body.in) + ','
        + compile(env, body.from) + ','
        + (body.to ? compile(env, body.to) : 'undefined') + ','
        + (body.step ? compile(env, body.step) : '1')
        + '))';
});

// ### Looping using for:
//
//      {for: {x: gen1, y: gen2,...}, 
//          when: cond, 
//          expr: value|[expr1, expr2, ...], 
//          where: {...}}
//      {for: {x: gen1, y: gen2,...}, 
//          when: cond, 
//          body: stmt|[stmt1, stmt2,...], 
//          where: {...}}
//
// The "expr" version produces an array with those values,
// whereas the "body" version is for side effects only.
define(Prim, 'for', function (env, expr) {
    return whereClause(env, expr, expr.where, function (env, expr) {
        var env2 = subenv(env);
        var envb = subenv(env2);
        var iters = Object.keys(expr.for);
        return '(function () {'
            + iters.map(function (ivar) {
                var v = newvar(env2, ivar);
                var gen_v = 'gen_' + v; /* Use an extra "gen_" prefix 
                                         * for variables that hold
                                         * generators. 
                                         */

                return 'var ' + v + ', ' + gen_v + ' = ' + compile(env2, expr.for[ivar]) + ';';
              }).join('')
            + (expr.expr ? 'var __result = [];' : '')
            + ('\nfunction __body('
                    + newvars(envb, iters).join(',')
                    + ') {'
                    + (expr.expr 
                        ? ('__result.push((' + compile_args(envb, expr.expr) + '));')
                        : ('(' + compile_args(envb, expr.body) + ')'))
                    + '}\n')
            + iters.map(function (ivar) {
                var v = varname(env2, ivar);
                var gen_v = 'gen_' + v;
                return '\nfor(' + v + ' = ' + gen_v + '(true);'
                    + v + ' !== undefined; '
                    + v + ' = ' + gen_v + '()) {';
              }).join('')
            + (expr.when 
                    ? ('if (' + compile(env2, expr.when) + ') {')
                    : '')
            + ('__body(' + varnames(env2, iters).join(',') + ');')
            + (expr.when ? '}' : '')
            + iters.map(function (ivar) { return '\n}'; }).join('')
            + (expr.expr ? '\nreturn __result;' : '')
            + '}())';
    });
});

Prim.run({for: {x: {from: 1, to: 4}, 
                y: {from: 100, to: 104}}, 
    body: [{display: {$_: [{_$: "x"}, {_$: "y"}]}}]});

// ### Let's support some math as well.
// {expr: "x + y", where: {x: val1, y: val2}}
// The expression can only see the variables in the where clause.
// UNSAFE!
define(Prim, 'expr', function (env, expr) {
    if (!env.options.unsafe) {
        throw "Unsafe expression! " + JSON.stringify(expr);
    }
    if (expr.where) {
        var vars = Object.keys(expr.where);
        return '(function (' + vars.join(',') + ') {'
            + 'return (' + expr.expr + ');}'
        + '(' 
        + vars.map(function (v) { return compile(env, expr.where[v]); }).join(',')
        + '))';
    } else {
        return '(' + expr.expr + ')';
    }
});

// ### Some higher order functions?

// {map: fn, list: listval}
define(Prim, 'map', function (env, expr) {
    return '(' + compile(env, expr.list) + '.map(' + compile(env, expr.map) + '))';
});

// {reduce: fn, list: listval, init: value}
define(Prim, 'reduce', function (env, expr) {
    return '(' + compile(env, expr.list) + '.reduce(' 
            + compile(env, expr.reduce) + ', '
            + compile(env, expr.init)
            + '))';
});

// {filter: fn, list: listval}
define(Prim, 'filter', function (env, expr) {
    return '(' + compile(env, expr.list) + '.filter(' + compile(env, expr.filter) + '))';
});

// ### Dot notation
// It is useful to refer to object parts directly using
// the dot notation. Just change lookupSymbol to directly
// support it.
lookupSymbol = (function (lookup) {
    var forbiddenProperties = {};
    return function (env, sym) {
        var parts = sym.split('.');
        if (parts.length === 1) {
            return lookup(env, sym);
        } else {
            parts[0] = lookup(env, parts[0]);
            parts.forEach(function (p,i) {
                if (i > 0) {
                    if (forbiddenProperties[p]) {
                        throw "Forbidden javascript property '" + p + "' accessed!";
                    }
                }
            });
            if (parts[0]) {
                return parts.join('.');
            } else {
                return undefined;
            }
        }
    };
}(lookupSymbol));

Prim.run({let: {x: {table: {cat: {$: "meow"}}}},
            in: {display: "x.cat"}});

// Try the lambda example again with dot notation access.
// Lets now try a lambda hello world.
Prim.run({let: {greet: {lambda: ["msg"],
                            body: [
                                {display: {$: "Hello lambda world ..."}},
                                {display: "msg"},
                                {display: "keywords.lockword"}
                            ]}},
            in: [{greet: [{$: "Planet earth rocks!"}], 
                    lockword: {$: "haha!"}}]});

// ## Defines and blocks
// It will certainly be convenient to be able to write do blocks
// for walking through steps and introduce definitions along the way,
// process them etc. A simple macro for that would work on --
//
//      {do: [stmt1, stmt2, ...],
//          where: {...}}
//
// and allow define statements in the mix, like this -
//
//      {define: {name1: value1, name2, value2,...}}
//
// We translate such a "do" block into a 
//      (function () {...}())
// form.
define(Prim, 'do', function (env, expr) {
    return whereClause(env, expr, expr.where, function (env, expr) {
        var result = '(function () {';
        expr.do.forEach(function (stmt, i) {
            if (stmt && operatorName(stmt) === 'define') {
                env = subenv(env); /* It is a define statement. Make a new environment.
                                    * This is an important step to ensure that new
                                    * definitions don't override older ones.
                                    */

                Object.keys(stmt.define).forEach(function (varname) {
                    result += 'var ' + newvar(env, varname) + ' = ';
                    result += compile(env, stmt.define[varname]) + ';';
                });
            } else {
                result += (i+1 < expr.do.length ? '' : 'return ')
                    + compile(env, stmt) + ';';
            }
        });
        return result + '}())';
    });
});

Prim.run({do: [
    {define: {x: 5}},
    {define: {fn: {lambda: ["y"], body: [{table: {x: "x", y: "y"}}]}}},
    {define: {x: 10}},
    {display: {fn: [10]}},
    {display: "x"}
]});

// ### Accessors
// We don't have any accessor functions for working with 
// object and array properties yet. Let's add a general purpose
// "get" and "put".

//      {get: [obj, key1, key2, ...]}
define(Prim, 'get', function (env, expr) {
    return expr.get.map(function (e, i) {
            var ce = compile(env, e);
            return (i > 0 ? ('['+ce+']') : ce);
          }).join('');
});

//      {put: [obj, key1, key2, ...], value: val}
define(Prim, 'put', function (env, expr) {
    if (expr.put.constructor === String) {
        return '(' + compile(env, expr.put) + ' = ' + compile(env, expr.value) + ')';
    } else if (expr.put.constructor === Array) {
        return '(' 
            + lookupSymbol(env, 'get')(env, expr.put) 
            + ' = ' 
            + compile(env, expr.value) 
            + ')';
    }
});

// ### Resolving power differences
//
// There is a asymmery between lambda and macro that is uncomfortable.
// It is that using a lambda always requires its arguments to be 
// wrapped into an array (other than keywords) whereas macros are able
// to work with free forms better. Ideally, they shouldn't have differences
// in form at usage time and should be able to work with all forms. 
// One simple solution to this is to auto-promote single non-array 
// arguments into one-element arrays at call time. We patch compile_apply 
// to resolve this.
//
// With this patch, you can have the following lambda -
//
//      {let: {ruler: {lambda: ["arg"],
//                      body: [{if: "keywords.double_rule",
//                              then: {display: {$: "================="}}
//                              else: {display: {$: "-----------------"}}},
//                             {display: "arg"}]}} 
//          ...}
//
// which can be called like this -
//
//      {ruler: {$: "An important message"}, double_rule: true}
//
// and "applied" like this -
//
//      {apply: "ruler", 
//          args: {list: [{$: "An important message"}]}, 
//          keywords: {table: {double_rule: true}}}
//
var compile_apply = (function (prevCompileApply) {
    return function (env, op, jexpr) {
        var opname = operatorName(jexpr);
        var head = jexpr[opname];
        if (head && head.constructor === Array) {
            return prevCompileApply(env, op, jexpr); // Safe. Old behaviour applies.
        } else {
            jexpr[opname] = [jexpr[opname]]; /* Transform the main argument into a 
                                              * one-element array.
                                              * HACK: We hack this by destructively modifying 
                                              * jexpr since the next time around we won't then 
                                              * get into this branch.
                                              */
            return prevCompileApply(env, op, jexpr);
        }
    };
}(compile_apply));

Prim.run({let: {ruler: {lambda: ["arg"],
                        body: [{if: "keywords.double_rule",
                                then: {display: {$: "======================="}},
                                else: {display: {$: "-----------------------"}}},
                               {display: "arg"}]}},
           in: [{ruler: {$: "An important message!"}, double_rule: true}]});

// This uniformity lets us turn 'display' into a function much more simply!
define(Prim, 'display', 'console.log');

// Can we turn map/reduce/filter into functions as well?
// This looks possible, but I'm not sure about the resulting
// efficiency, so I'll leave them as macros for now and leave
// it to YOU to figure that out.
//
//      define(Prim, 'map', '(function (fn) { return this.list.map(fn); })');
//      define(Prim, 'reduce', '(function (fn) { return this.list.reduce(fn, this.init); })');
//      define(Prim, 'filter', '(function (fn) { return this.list.filter(fn); })');
//
// Many others that we've written as macros should similarly be
// expressed as functions .. except for such runtime performance considerations.
// The disadvantage to how we've been doing this up to here, is
// that we cannot use the macros with "apply" in a program. That's
// a pretty BIG disadvantage, but I'm waving my hands and saying
// "you can always wrap a lambda around it" :)
//
// Have fun!


// ## A runtime environment?
// So far, we don't have the notion of a runtime and all "functions"
// are actually macros and all is not well in this world just yet.
// We need some way to provide an environment that exposes symbol
// bindings to some piece of compiled code that we then evaluate
// using eval().
//
// We use a very simple model of a language runtime - which is a
// function that takes in a piece of compiled code and evaluates
// it using eval! The function is free to introduce new bindings
// in its local environment which then become accessible to eval.
// In other words, we just treat "eval" itself as a runtime.
//
// Here is a sample runtime that redefines "map", "reduce"
// and "filter" as functions instead of the macros that we defined
// them to be. What is returned from a call to the runtime is
// a compiled Javascript function, which when you call will result
// in the expression being evaluated. This returned function is
// of the form -
//      function (param) { return something; }
// and you can pass in any object for the "param". The expression
// you supply will be able to safely access this object as the 
// direct symbol "param". If you omit this argument, then accessing
// "param" in your expression will result in "undefined".
//
// Take a look at the sample function definitions. They access
// the regular arguments through the usual JS arguments and access
// the keyword argument provided through "this".
function hofRT(parentEnv, expr) {

    var defs = {
        map: function (fn) {
            return this.list.map(fn);
        },

        reduce: function (fn) {
            return this.list.reduce(fn, this.init);
        },

        filter: function (fn) {
            return this.list.filter(fn);
        }
    };

    var env = subenv(parentEnv);

    Object.keys(defs).forEach(function (fn) {
        define(env, fn, "__runtime__." + fn);
    });

    return eval('(function (__runtime__) { return (function (' + newvar(env, 'param') + ') { '
                    + 'return (' + compile(env, expr) + ');'
                    + '}); })')(defs);
}

console.log("Testing map function in hofRT..");
console.log(hofRT(Prim, {map: {lambda: ["x"], body: {table: {x: "x"}}}, 
                         list: {list: [1,2,3]}})());

// The pattern expressed in hofRT can be encapsulated as a generic thing where
// you have a "runtime maker" function to which you pass in an object
// containing the definitions you want to make visible when running the
// code and you get back a function that can run expressions with those
// definitions. In this case, we make it so that calling the returned
// runtime function with an expression does not evaluate it like eval
// does, but compiles it and returns the compiled result as a function
// that you can then call as many times as you want.
//
// So the calling sequence goes like this --
//
//      var rt = J.runtime({...definitions...});
//      var proc = rt({...jexpr...});
//      proc(param1);
//      proc(param2);
//      ...
//      
function runtime(env, definitions) {
 
    var rtenv = subenv(env); // New compiler env holds the definitions.
    Object.keys(definitions).forEach(function (fn) {
        define(rtenv, fn, 'runtime$' + rtenv.id + '$.' + fn);
    });

    return function (expr) {
        var env = subenv(rtenv); // Make a new one so that each run is independent.

        return eval('(function (__runtime__) { return (function (' + newvar(env, 'param') + ') {'
                        + 'var runtime$' + rtenv.id + '$ = __runtime__;'
                        + 'return (' + compile(env, expr) + ');'
                        + '}); })')(definitions);
    };
}

// ## Standard library

// With the above notion of runtime, we can define a "standard library"
// that implements as functions some of what we wrote above as macros.
var standardLibrary = {
    display: function (thing) {
        console.log(thing);
        return thing;
    },

    from: function (fromIx) {
        var step = this.step === undefined ? 1 : this.step;
        var toIx = this.to === undefined ? (fromIx + step * 1e16) : this.to;
        var i = fromIx;

        return function (reset) {
            var result;

            if (reset) {
                i = fromIx;
            }

            if (step >= 0 ? (i < toIx) : (i > toIx)) {
                result = i;
                i += step;
                return result;
            } else {
                return undefined; // Indicates end of iteration.
            }
        };
    },

    in: function (arr) {
        var step = this.step === undefined ? 1 : this.step;
        var toIx = this.to === undefined ? (step >= 0 ? (fromIx + arr.length) : -1) : this.to;
        var i = fromIx;

        return function (reset) {
            var resultIx;

            if (reset) {
                i = fromIx;
            }

            if (step >= 0 ? (i < toIx) : (i > toIx)) {
                resultIx = i;
                i += step;
                return arr[resultIx];
            } else {
                return undefined; // Indicates end of iteration.
            }
        };
    },

    map: function (fn) {
        return this.list.map(fn);
    },

    reduce: function (fn) {
        return this.list.reduce(fn, this.init);
    },

    filter: function (fn) {
        return this.list.filter(fn);
    }
};

// ... but then we'll need some way of combining multiple
// such definitions lists into a single one before we can use
// the Env.prototype.runtime call to make a runtime. We'll also
// need to insert the standard definitions before any custom
// definitions are loaded. Let's therefore patch the runtime function 
// to accept multiple definitions objects and merge them all into a 
// single pile before making a runtime.
runtime = (function (runtime) {
    return function (env) {
        var definitions = copyValues(standardLibrary, {});
        copyValues(Array.prototype.slice.call(arguments, 1), definitions);
        return runtime(env, definitions);
    };
}(runtime))

function copyValues(source, target) {
    if (source.constructor === Array) {
        source.forEach(function (d) {
            copyValues(d, target);
        });
    } else if (source.constructor === Object) {
        Object.keys(source).forEach(function (k) {
            target[k] = source[k];
        });
    } else if (source.constructor === Function) {
        source(target); /* When a function is passed, I pass it the target
                         * and let it deal with inserting primitives. That
                         * way, the function can make use of what was defined
                         * before it was called, such as the standardLibrary.
                         */
                        
    }
    return target;
}

console.log("Testing standardLibrary..");
runtime(Prim)(
        {let: {},
            in: [{display: {map: {lambda: ["x"], body: {table: {x: "x"}}}, 
                            list: {list: [1,2,3]}}},
                 {display: {for: {x: {from: 1, to: 10}}, expr: {table: {x: "x"}}}}]}
        )();

// ## A *different* model of a runtime ##
//
// Actually, I don't quite like the above model of the runtime, because
// I can't now compile code in one place and run it in 
// another place. To fix that, I need some way to indicate that
// a symbol whose value is unknown at compile time is expected to
// be resolved at runtime. That's most easily done by patching
// lookupSymbol. Note that lookupSymbol will *always* succeed now
// for syntactically valid symbols. Also, we use the "R_" prefix just
// so we don't walk all over the JS proprietary properties.
lookupSymbol = (function (oldLookupSymbol) {
    var symRE = /^[a-zA-Z_\$][a-zA-Z0-9_\.\$]*$/;
    return function (env, sym) {
        return oldLookupSymbol(env, sym) 
                || (symRE.test(sym) ? ('__jexpr_runtime__.R_' + sym) : undefined);
    };
}(lookupSymbol));

// ... then I need to define a top level block compiler that will
// do the necessary wrapping.
function compile_to_js(env, exprArr) {
    return '(function (__jexpr_runtime__, ' + newvar(env, 'param') + ') {'
                + 'return (' + compile_args(env, exprArr) + ');'
                + '})';
}

// Now the runtime building can be independent of
// the compilation environment which may no longer exist. 
// Much cleaner!
function makeRuntime() {
    var definitions = copyValues(standardLibrary, {});
    copyValues(Array.prototype.slice.call(arguments, 0), definitions);
    var prefixed = {};
    Object.keys(definitions).forEach(function (k) {
        prefixed['R_' + k] = definitions[k];
    });
    return prefixed;
}

// Now the calling sequence is -
//
//      eval(compile_to_js(env, [expr..]))(makeRuntime(...), {...params...})
//
// Though this DRAMATICALLY alters how a runtime is defined, the
// actual definition of the runtime such as `standardLibrary` remains
// the same.

// Undefine the definitions moved to the standardLibrary so that
// they can be overridden by user runtime definitions.
Object.keys(standardLibrary).forEach(function (key) {
    define(Prim, key, undefined);
});

// Math functions are safe.
// FIXME: ... but actually not. Math.constructor and such stuff
// is now exposed to the language! This is actually a general
// problem with allowing the dot syntax without restrictions.
standardLibrary.Math = Math;

//
// ## Limiting exposure in the exports
//
// I'd like the ability to be very very selective about what 
// gets exposed in the environment so that at some point I can
// safely expose compilation environments at runtime. To do this,
// I create a "frozen wrapper" around a given environment that poses
// no extra running overhead for the internal machinery.
//
// The exposed functionality is as follows -
//
// `J` is the name of the exposed variable containing this API.
//
// `J.subenv()` makes a new environment with J as its parent. That new
// sub-environment also gets this very same API.
//
// `J.option(name, [value])` gets/sets environment options. Currently the
// only option exposed is 'unsafe' which can be set to true/false to permit
// unsafe expressions at compilation time.
//
// `J.define(name, value)` puts a symbol definition into the compilation
// environment. Defining symbols in a sub-environment does not affect
// symbol lookup in parent environments.
//
// `J.compile_to_js(expr,...)` Returns the compiled Javascript source for the
// given expression as a string. Evaluating this string will give you a function 
// of the form --
//
//      function (runtime, param) {..}
// 
// `J.compile(expr,...)` Returns the compiled closure that you can pass to `J.eval`,
// or call yourself.
//
// `J.runtime(defns...)` will collect all the supplied runtime definitions
// into a single object and return it. The `standardLibrary` is included
// by default.
//
// Specifying definitions has a lot of flexibility -
//
//  1. You can give a table of name->defn mappings,
//  2. You can give a function(table) which is then
//     applied to the table of already loaded definitions
//     so you can add new ones that make use of older ones.
//  3. You can pass an array of such tables or functions
//     and it steps through such arrays recursively. This
//     helps with "componentizing" the runtime.
//
// `J.eval(expr, runtime, param)` wraps it all together. You can either
// pass in a compiled expression (as a closure in the form returned by
// `J.compile(expr)` or a j-expression which will then be compiled and
// evaluated. If given  a j-expression, this is equivalent to -
//      eval(J.compile(expr))(runtime, param)
//
function freeze(env) {
    return Object.freeze({
        subenv: function () {
            return freeze(subenv(env));
        },
        option: function (optName, optVal) {
            return (arguments.length === 1 
                        ? env.options[optName] 
                        : (env.options[optName] = optVal));
        },
        define: function (name, value) {
            return define(env, name, value);
        },
        compile_to_js: function () {
            return compile_to_js(env, Array.prototype.slice.call(arguments, 0));
        },
        compile: function () {
            return eval(compile_to_js(env, Array.prototype.slice.call(arguments, 0)));
        },
        runtime: makeRuntime,
        eval: function (expr, runtime, param) {
            runtime = runtime || makeRuntime();
            param = param || {};
            if (expr.constructor === Function) {
                return expr(runtime, param); // Already a compiled expression.
            } else {
                return eval(compile_to_js(env, [expr]))(runtime, param); // Need to compile.
            }
        }
    });
}

// for debugging.
function show(label, x) {
    console.log(label + ':\t' + JSON.stringify(x));
    return x;
}

return freeze(Prim);

}());

// Are we in node.js? If so set the exports variable.
// Otherwise just shut up and return.
try {
    module.exports = J;
} catch (e) {
}
