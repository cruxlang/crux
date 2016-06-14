(function() {
  "use strict";
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
;
  var $array_get;
  var $array_len;
  var $array_replicate;
  var $array_each;
  var $array_sliceFrom;
  var $array_sliceTo;
  var $array_slice;
  (function() {
    var $0 = function (len) { return new Array(len); };
    var _unsafe_new = $0;
    var $1 = function (arr, idx, el) { arr[idx] = el; };
    var _unsafe_set = $1;
    var $2 = function (arr, idx) { return arr[idx]; };
    var _unsafe_get = $2;
    function get(a, idx) {
      var $3 = _unsafe_get(a, idx);
      return $3;
    }
    function len(a) {
      var $4 = a;
      var $5 = ($4).length;
      return $5;
    }
    function replicate(element, length) {
      var $6 = _unsafe_new(length);
      {
        var arr = $6;
      }
      {
        var i = 0;
      }
      while (true)
      {
        var $7 = (i<length);
        var $8 = (!$7);
        var $9;
        if ($8) {
          break;
        }
        else {
          $9 = (void 0);
        }
        var $10 = _unsafe_set(arr, i, element);
        var $11 = (i+1);
        i = $11;
      }
      return arr;
    }
    function each(arr, f) {
      {
        var i = 0;
      }
      var $12 = len(arr);
      {
        var length = $12;
      }
      while (true)
      {
        var $13 = (i<length);
        var $14 = (!$13);
        var $15;
        if ($14) {
          break;
        }
        else {
          $15 = (void 0);
        }
        var $16 = _unsafe_get(arr, i);
        var $17 = f($16);
        var $18 = (i+1);
        i = $18;
      }
      return (void 0);
    }
    function sliceFrom(arr, start) {
      var $19 = arr;
      var $20 = ($19).slice(start);
      return $20;
    }
    function sliceTo(arr, end) {
      var $21 = arr;
      var $22 = ($21).slice(0, end);
      return $22;
    }
    function slice(arr, start, end) {
      var $23 = arr;
      var $24 = ($23).slice(start, end);
      return $24;
    }
    $array_get = get;
    $array_len = len;
    $array_replicate = replicate;
    $array_each = each;
    $array_sliceFrom = sliceFrom;
    $array_sliceTo = sliceTo;
    $array_slice = slice;
  })();
  var $boolean_True;
  var $boolean_False;
  var $boolean_not;
  (function() {
    var True = true;
    var False = false;
    function not(b) {
      var $0;
      if ((true===b)) {
        {
        }
        $0 = False;
      }
      else {
        if ((false===b)) {
          {
          }
          $0 = True;
        }
        else {
        }
      }
      return $0;
    }
    $boolean_True = True;
    $boolean_False = False;
    $boolean_not = not;
  })();
  var $option_Some;
  var $option_None;
  var $option_eq;
  (function() {
    function Some(a0) {
      return ["Some", a0];
    }
    var None = ["None"];
    function eq(lhs, rhs) {
      var $0;
      if (("Some"===lhs[0])) {
        {
          var a = lhs[1];
        }
        var $1;
        if (("Some"===rhs[0])) {
          {
            var b = rhs[1];
          }
          var $2 = (a===b);
          $1 = $2;
        }
        else {
          {
          }
          $1 = $boolean_False;
        }
        $0 = $1;
      }
      else {
        if (("None"===lhs[0])) {
          {
          }
          var $3;
          if (("Some"===rhs[0])) {
            {
            }
            $3 = $boolean_False;
          }
          else {
            if (("None"===rhs[0])) {
              {
              }
              $3 = $boolean_True;
            }
            else {
            }
          }
          $0 = $3;
        }
        else {
        }
      }
      return $0;
    }
    $option_Some = Some;
    $option_None = None;
    $option_eq = eq;
  })();
  var $result_Ok;
  var $result_Err;
  (function() {
    function Ok(a0) {
      return ["Ok", a0];
    }
    function Err(a0) {
      return ["Err", a0];
    }
    $result_Ok = Ok;
    $result_Err = Err;
  })();
  var $mutarray_append;
  var $mutarray_get;
  var $mutarray_len;
  var $mutarray_replicate;
  var $mutarray_each;
  var $mutarray_sliceFrom;
  var $mutarray_sliceTo;
  var $mutarray_slice;
  var $mutarray_freeze;
  var $mutarray_sort;
  (function() {
    var $0 = function (len) { return new Array(len); };
    var _unsafe_new = $0;
    var $1 = function (arr, idx, el) { arr[idx] = el; };
    var _unsafe_set = $1;
    var $2 = function (arr, idx) { return arr[idx]; };
    var _unsafe_get = $2;
    function append(a, v) {
      var $3 = a;
      var $4 = ($3).push(v);
      return $4;
    }
    function get(a, idx) {
      var $5 = _unsafe_get(a, idx);
      return $5;
    }
    function len(a) {
      var $6 = a;
      var $7 = ($6).length;
      return $7;
    }
    function replicate(element, length) {
      var $8 = _unsafe_new(length);
      {
        var arr = $8;
      }
      {
        var i = 0;
      }
      while (true)
      {
        var $9 = (i<length);
        var $10 = (!$9);
        var $11;
        if ($10) {
          break;
        }
        else {
          $11 = (void 0);
        }
        var $12 = _unsafe_set(arr, i, element);
        var $13 = (i+1);
        i = $13;
      }
      return arr;
    }
    function each(arr, f) {
      {
        var i = 0;
      }
      var $14 = len(arr);
      {
        var length = $14;
      }
      while (true)
      {
        var $15 = (i<length);
        var $16 = (!$15);
        var $17;
        if ($16) {
          break;
        }
        else {
          $17 = (void 0);
        }
        var $18 = _unsafe_get(arr, i);
        var $19 = f($18);
        var $20 = (i+1);
        i = $20;
      }
      return (void 0);
    }
    function sliceFrom(arr, start) {
      var $21 = arr;
      var $22 = ($21).slice(start);
      return $22;
    }
    function sliceTo(arr, end) {
      var $23 = arr;
      var $24 = ($23).slice(0, end);
      return $24;
    }
    function slice(arr, start, end) {
      var $25 = arr;
      var $26 = ($25).slice(start, end);
      return $26;
    }
    function freeze(arr) {
      var $27 = arr;
      var $28 = ($27).slice();
      return $28;
    }
    function sort(arr) {
      var $29 = arr;
      var $30 = ($29).sort();
      return $30;
    }
    $mutarray_append = append;
    $mutarray_get = get;
    $mutarray_len = len;
    $mutarray_replicate = replicate;
    $mutarray_each = each;
    $mutarray_sliceFrom = sliceFrom;
    $mutarray_sliceTo = sliceTo;
    $mutarray_slice = slice;
    $mutarray_freeze = freeze;
    $mutarray_sort = sort;
  })();
  var $string_length;
  var $string_startsWith;
  var $string_endsWith;
  var $string_join;
  var $string_sliceFrom;
  var $string_trim;
  (function() {
    function length(s) {
      var $0 = s;
      var $1 = ($0).length;
      return $1;
    }
    function startsWith(haystack, needle) {
      var $2 = haystack;
      {
        var h = $2;
      }
      var $3 = needle;
      {
        var n = $3;
      }
      var $4 = (n).length;
      var $5 = (h).length;
      var $6 = ($4>$5);
      var $7;
      if ($6) {
        return $boolean_False;
      }
      else {
        $7 = (void 0);
      }
      var $8 = (h).indexOf(n, 0);
      var $9 = ($8===0);
      return $9;
    }
    function endsWith(haystack, needle) {
      var $10 = haystack;
      {
        var h = $10;
      }
      var $11 = needle;
      {
        var n = $11;
      }
      var $12 = (n).length;
      var $13 = (h).length;
      var $14 = ($12>$13);
      var $15;
      if ($14) {
        return $boolean_False;
      }
      else {
        $15 = (void 0);
      }
      var $16 = (h).length;
      var $17 = (n).length;
      var $18 = ($16-$17);
      var $19 = (h).indexOf(n, $18);
      var $20 = (0-1);
      var $21 = ($19!==$20);
      return $21;
    }
    function join(sep, elements) {
      var $22 = elements;
      var $23 = ($22).join(sep);
      return $23;
    }
    function sliceFrom(s, start) {
      var $24 = s;
      var $25 = ($24).slice(start);
      return $25;
    }
    function trim(s) {
      var $26 = s;
      var $27 = ($26).trim();
      return $27;
    }
    $string_length = length;
    $string_startsWith = startsWith;
    $string_endsWith = endsWith;
    $string_join = join;
    $string_sliceFrom = sliceFrom;
    $string_trim = trim;
  })();
  var $builtin_print;
  var $builtin_toString;
  var $builtin_emptyArray;
  (function() {
    function print(a) {
      var $0 = console;
      var $1 = ($0).log(a);
      return $1;
    }
    var $2 = function toString(v) { return '' + v; };
    var toString = $2;
    function emptyArray() {
      var $3 = [];
      return $3;
    }
    $builtin_print = print;
    $builtin_toString = toString;
    $builtin_emptyArray = emptyArray;
  })();
  (function() {
    function TimerId(a0) {
      return ["TimerId", a0];
    }
    function unTimerId($_0) {
      var t = $_0[1];
      return t;
    }
    var COMPILE_DELAY = 1000;
    function as_bool(a) {
      var $0 = function (x) { return !!x; };
      {
        var to_bool = $0;
      }
      var $1 = a;
      var $2 = to_bool($1);
      return $2;
    }
    function querySelector(sel) {
      var $3 = document;
      var $4 = ($3).querySelector(sel);
      return $4;
    }
    function getElementById(id) {
      var $5 = document;
      var $6 = ($5).getElementById(id);
      return $6;
    }
    function setTimeout(f, delay) {
      var $7 = window;
      var $8 = ($7).setTimeout(f, delay);
      var $9 = TimerId($8);
      return $9;
    }
    function clearTimeout(tid) {
      var $10 = window;
      var $11 = unTimerId(tid);
      var $12 = ($10).clearTimeout($11);
      return $12;
    }
    function compileCrux(source) {
      var $13 = window;
      var $14 = ($13).compileCrux(source);
      {
        var rawRes = $14;
      }
      var $15 = (rawRes).error;
      var $16 = as_bool($15);
      var $17;
      if ($16) {
        var $18 = (rawRes).error;
        var $19 = $result_Err($18);
        $17 = $19;
      }
      else {
        var $20 = (rawRes).result;
        var $21 = $result_Ok($20);
        $17 = $21;
      }
      return $17;
    }
    function newXmlHttpRequest() {
      var $22 = new XMLHttpRequest;
      return $22;
    }
    function toJson(o) {
      var $23 = JSON.stringify;
      var $24 = $23(o);
      return $24;
    }
    function parseJson(s) {
      var $25 = JSON.parse;
      var $26 = $25(s);
      return $26;
    }
    var Idle = ["Idle"];
    function Waiting(a0) {
      return ["Waiting", a0];
    }
    function Optimizing(a0) {
      return ["Optimizing", a0];
    }
    function LastCompile(a0, a1) {
      return ["LastCompile", a0, a1];
    }
    function Compiler(a0) {
      return ["Compiler", a0];
    }
    function newCompiler(onresult) {
      var $27 = Compiler({state:Idle, lastCompile:$option_None, onresult:onresult});
      return $27;
    }
    function compile($_1, source, optimize) {
      var this$ = $_1[1];
      var $28 = (this$).state;
      var $29;
      if (("Idle"===$28[0])) {
        {
        }
        $29 = (void 0);
      }
      else {
        if (("Waiting"===$28[0])) {
          {
            var tid = $28[1];
          }
          var $30 = clearTimeout(tid);
          $29 = $30;
        }
        else {
          if (("Optimizing"===$28[0])) {
            {
              var xhr = $28[1];
            }
            var $31 = (xhr).abort();
            $29 = $31;
          }
          else {
          }
        }
      }
      (this$).state = Idle;
      var $32 = (this$).lastCompile;
      var $33 = LastCompile(source, optimize);
      var $34 = $option_Some($33);
      var $35 = $option_eq($32, $34);
      var $36;
      if ($35) {
        return (void 0);
      }
      else {
        $36 = (void 0);
      }
      var $68 = setTimeout((function() {
        var $37 = compileCrux(source);
        var $38;
        if (("Err"===$37[0])) {
          {
            var error = $37[1];
          }
          (this$).state = Idle;
          var $39 = LastCompile(source, optimize);
          var $40 = $option_Some($39);
          (this$).lastCompile = $40;
          var $41 = ("Compile error:\n"+error);
          var $42 = $result_Err($41);
          var $43 = (this$).onresult($42);
          return (void 0);
        }
        else {
          if (("Ok"===$37[0])) {
            {
              var res = $37[1];
            }
            var $44 = $boolean_not(optimize);
            var $45;
            if ($44) {
              (this$).state = Idle;
              var $46 = LastCompile(source, optimize);
              var $47 = $option_Some($46);
              (this$).lastCompile = $47;
              var $48 = $result_Ok(res);
              var $49 = (this$).onresult($48);
              return (void 0);
            }
            else {
              $45 = res;
            }
            $38 = $45;
          }
          else {
          }
        }
        {
          var result = $38;
        }
        var $50 = newXmlHttpRequest();
        {
          var xhr = $50;
        }
        var $51 = (xhr).open("POST", "https://crux-closure-service.herokuapp.com/compile");
        var $52 = (xhr).setRequestHeader("content-type", "application/json");
        (xhr).timeout = 60000;
        var $53 = toJson({source:result});
        var $54 = (xhr).send($53);
        var $55 = Optimizing(xhr);
        (this$).state = $55;
        (xhr).onload = (function() {
          var $56 = (xhr).response;
          var $57 = parseJson($56);
          {
            var result2 = $57;
          }
          (this$).state = Idle;
          var $58 = LastCompile(source, optimize);
          var $59 = $option_Some($58);
          (this$).lastCompile = $59;
          var $60 = (result2).source;
          var $61 = $result_Ok($60);
          var $62 = (this$).onresult($61);
          return $62;
        });
        (xhr).onerror = (function(e) {
          (this$).lastCompile = $option_None;
          var $63 = ("Network error:\n"+e);
          var $64 = $result_Err($63);
          var $65 = (this$).onresult($64);
          return $65;
        });
        (xhr).ontimeout = (function() {
          (this$).lastCompile = $option_None;
          var $66 = $result_Err("Network timeout");
          var $67 = (this$).onresult($66);
          return $67;
        });
        return (void 0);
      }), COMPILE_DELAY);
      var $69 = Waiting($68);
      (this$).state = $69;
      return (void 0);
    }
    function main() {
      var $70 = querySelector(".crux-playground .source");
      {
        var sourceTextArea = $70;
      }
      var $71 = querySelector(".crux-playground .output");
      {
        var outputTextArea = $71;
      }
      var $72 = querySelector(".crux-playground .optimize");
      {
        var optimizeCheckbox = $72;
      }
      var $73 = querySelector(".crux-playground .run");
      {
        var runButton = $73;
      }
      {
        var loadExampleSource = (function() {
          var $74 = getElementById("initial_example");
          var $75 = ($74).text;
          {
            var source = $75;
          }
          var $76 = $string_trim(source);
          source = $76;
          (sourceTextArea).value = source;
          return (void 0);
        });
      }
      var $82 = newCompiler((function(result) {
        var $77;
        if (("Ok"===result[0])) {
          {
            var res = result[1];
          }
          var $78 = (outputTextArea).classList;
          var $79 = ($78).remove("has-errors");
          (outputTextArea).value = res;
          $77 = (void 0);
        }
        else {
          if (("Err"===result[0])) {
            {
              var err = result[1];
            }
            var $80 = (outputTextArea).classList;
            var $81 = ($80).add("has-errors");
            (outputTextArea).value = err;
            $77 = (void 0);
          }
          else {
          }
        }
        return $77;
      }));
      {
        var compiler = $82;
      }
      {
        var recompile = (function() {
          var $83 = (sourceTextArea).value;
          {
            var content = $83;
          }
          var $84 = (optimizeCheckbox).checked;
          {
            var optimize = $84;
          }
          var $85 = compile(compiler, content, optimize);
          return $85;
        });
      }
      {
        var registerCompileListener = (function() {
          var $86 = (sourceTextArea).addEventListener("input", recompile);
          var $87 = (optimizeCheckbox).addEventListener("change", recompile);
          return $87;
        });
      }
      var $88 = loadExampleSource();
      var $89 = registerCompileListener();
      var $90 = recompile();
      (sourceTextArea).disabled = $boolean_False;
      var $91 = (sourceTextArea).setSelectionRange(0, 0);
      var $92 = (sourceTextArea).focus();
      var $94 = (runButton).addEventListener("click", (function() {
        var $93 = $builtin_print("run button temporarily disabled");
        return $93;
      }));
      return $94;
    }
    var $95 = main();
  })();
})();
