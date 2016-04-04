var _rts_current_exception = null;

function _rts_set_exception(e) {
    _rts_current_exception = e;
}

function _rts_clear_exception() {
    _rts_current_exception = null;
}

function _rts_new_exception(name, baseException) {
    // TODO: _rts_create_named_function
    var errorClass = function(message) {
        this.name = name;
        this.message = message;
        // we could fix up this.stack too
    };

    // TODO: when necessary
    errorClass.prototype.constructor = errorClass;

    return errorClass;
}
