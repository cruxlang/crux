var _rts_current_exception = null;

function _rts_set_exception(e) {
    _rts_current_exception = e;
}

function _rts_clear_exception() {
    _rts_current_exception = null;
}

function _rts_new_exception(name, baseException) {
    // TODO: validate the name is a valid js identifier
    // TODO: catch the eval exception in CSP contexts and try something simpler in that case
    var ctor = new Function("name", "return function " + name + "(message, e) {\n  this.message = message;\n  this.buildStack(e || new Error);\n}\n")(name);

    ctor.prototype.name = name;
    ctor.prototype.toString = function() {
        if (this.message === undefined) {
            return this.name;
        } else {
            return this.name + ": " + this.message;
        }
    };
    ctor.prototype.buildStack = function(e) {
        var stack = e.stack;
        if (stack !== undefined) {
            this.stack = this.toString() + '\n' +
                stack.replace(/^Error(:[^\n]*)?\n/, '');
        }
    };

    ctor.throw = function(message, e) {
        throw new ctor(message, e);
    };

    ctor.check = function(e) {
        return e instanceof ctor;
    };

    return ctor;
}

var _rts_exports = typeof exports === 'undefined' ? {} : exports;

function _rts_show_string(s) {
    // TODO: There have to be faster ways to implement this.  For example,
    // do all browsers optimize string concatenation?  Would it make sense to do
    // a fast scan for escapable characters before switching to char-by-char
    // appends?
    let escapes = {
        '\0': '0',
        '\\': '\\',
        '\"': '"',
        '\?': '?',
        '\'': "'",
        '\a': 'a',
        '\b': 'b',
        '\f': 'f',
        '\r': 'r',
        '\n': 'n',
        '\t': 't',
        '\v': 'v',
    };
    let rv = '"';
    for (const code of s) {
        let escape = escapes[code];
        let codePoint = code.codePointAt();
        if (escape) {
            rv += '\\' + escape;
        } else if (codePoint < 0x10) {
            rv += '\\x0' + codePoint;
        } else if (codePoint < 0x20) {
            rv += '\\x1' + (codePoint - 0x10);
        } else {
            rv += code;
        }
    }
    return rv + '"';
}