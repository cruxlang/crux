local _rts_current_exception = nil

function _rts_set_exception(e)
    _rts_current_exception = e
end

function _rts_clear_exception()
    _rts_current_exception = nil
end

function _rts_new_exception(name, baseException)
    -- TODO: validate the name is a valid js identifier
    -- TODO: catch the eval exception in CSP contexts and try something simpler in that case
    -- local ctor = new Function("name", "return function " + name + "(message, e) {\n  this.message = message;\n  this.buildStack(e || new Error);\n}\n")(name);
    local ctor = {}

    -- ctor.prototype.name = name;
    -- ctor.prototype.toString = function()
    --     if (this.message === undefined) then
    --         return this.name;
    --     } else {
    --         return this.name + ": " + this.message;
    --     }
    -- };
    -- ctor.prototype.buildStack = function(e) {
    --     local stack = e.stack;
    --     if (stack !== undefined) {
    --         this.stack = this.toString() + '\n' +
    --             stack.replace(/^Error(:[^\n]*)?\n/, '');
    --     }
    -- };

    -- ctor.throw = function(message, e) {
    --     throw new ctor(message, e);
    -- };

    -- ctor.check = function(e) {
    --     return e instanceof ctor;
    -- };

    return ctor;
}

local _rts_exports = typeof(exports) == 'nil' and {} or exports

function _rts_show_string(s)
    -- TODO: There have to be faster ways to implement this.  For example,
    -- do all browsers optimize string concatenation?  Would it make sense to do
    -- a fast scan for escapable characters before switching to char-by-char
    -- appends?
    local escapes = {
        '\0' = '0',
        '\\' = '\\',
        '\"' = '"',
        '\?' = '?',
        '\'' = "'",
        '\a' = 'a',
        '\b' = 'b',
        '\f' = 'f',
        '\r' = 'r',
        '\n' = 'n',
        '\t' = 't',
        '\v' = 'v',
    };
    local rv = '"'
    for s, escape in pairs(escapes) do
        local codePoint = code.codePointAt();
        if escape then
            rv = rv .. '\\' .. escape
        else if codePoint < 0x10 then
            rv = rv .. '\\x0' .. codePoint
        else if codePoint < 0x20 then
            rv = rv .. '\\x1' .. (codePoint - 0x10)
        else {
            rv = rv .. code
        }
    }
    return rv + '"';
end
