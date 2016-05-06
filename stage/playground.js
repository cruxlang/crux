(function() {
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
    function replicate(element, len) {
      var $6 = _unsafe_new(len);
      {
        var arr = $6;
      }
      {
        var i = 0;
      }
      while (true)
      {
        var $7 = (i<len);
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
    function replicate(element, len) {
      var $8 = _unsafe_new(len);
      {
        var arr = $8;
      }
      {
        var i = 0;
      }
      while (true)
      {
        var $9 = (i<len);
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
    $string_length = length;
    $string_startsWith = startsWith;
    $string_endsWith = endsWith;
    $string_join = join;
    $string_sliceFrom = sliceFrom;
  })();
  var $builtin_True;
  var $builtin_False;
  var $builtin_not;
  var $builtin_print;
  var $builtin_toString;
  var $builtin_Some;
  var $builtin_None;
  var $builtin_emptyArray;
  var $builtin_get;
  var $builtin_len;
  var $builtin_replicate;
  var $builtin_each;
  var $builtin_map;
  var $builtin_sliceFrom;
  var $builtin_sliceTo;
  var $builtin_slice;
  (function() {
    var True = $boolean_True;
    var False = $boolean_False;
    var not = $boolean_not;
    function print(a) {
      var $0 = console;
      var $1 = ($0).log(a);
      return $1;
    }
    var $2 = function toString(v) { return '' + v; };
    var toString = $2;
    function Some(a0) {
      return ["Some", a0];
    }
    var None = ["None"];
    function emptyArray() {
      var $3 = [];
      return $3;
    }
    var get = $array_get;
    var len = $array_len;
    var replicate = $array_replicate;
    var each = $array_each;
    function map(f, a) {
      var $4 = [].map;
      var $5 = ($4).call(a, f);
      return $5;
    }
    var sliceFrom = $array_sliceFrom;
    var sliceTo = $array_sliceTo;
    var slice = $array_slice;
    $builtin_True = True;
    $builtin_False = False;
    $builtin_not = not;
    $builtin_print = print;
    $builtin_toString = toString;
    $builtin_Some = Some;
    $builtin_None = None;
    $builtin_emptyArray = emptyArray;
    $builtin_get = get;
    $builtin_len = len;
    $builtin_replicate = replicate;
    $builtin_each = each;
    $builtin_map = map;
    $builtin_sliceFrom = sliceFrom;
    $builtin_sliceTo = sliceTo;
    $builtin_slice = slice;
  })();
  (function() {
    function Ok(a0) {
      return ["Ok", a0];
    }
    function Err(a0) {
      return ["Err", a0];
    }
    function TimerId(a0) {
      return ["TimerId", a0];
    }
    function unTimerId(tid) {
      var $0;
      if (("TimerId"===tid[0])) {
        {
          var t = tid[1];
        }
        $0 = t;
      }
      else {
      }
      return $0;
    }
    var COMPILE_DELAY = 1000;
    function as_bool(a) {
      var $1 = function (x) { return !!x; };
      {
        var to_bool = $1;
      }
      var $2 = a;
      var $3 = to_bool($2);
      return $3;
    }
    function querySelector(sel) {
      var $4 = document;
      var $5 = ($4).querySelector(sel);
      return $5;
    }
    function getElementById(id) {
      var $6 = document;
      var $7 = ($6).getElementById(id);
      return $7;
    }
    function setTimeout(f, delay) {
      var $8 = window;
      var $9 = ($8).setTimeout(f, delay);
      var $10 = TimerId($9);
      return $10;
    }
    function clearTimeout(tid) {
      var $11 = window;
      var $12 = unTimerId(tid);
      var $13 = ($11).clearTimeout($12);
      return $13;
    }
    function compileCrux(source) {
      var $14 = window;
      var $15 = ($14).compileCrux(source);
      {
        var rawRes = $15;
      }
      var $16 = (rawRes).error;
      var $17 = as_bool($16);
      var $18;
      if ($17) {
        var $19 = (rawRes).error;
        var $20 = Err($19);
        $18 = $20;
      }
      else {
        var $21 = (rawRes).result;
        var $22 = Ok($21);
        $18 = $22;
      }
      return $18;
    }
    function newXmlHttpRequest() {
      var $23 = new XMLHttpRequest;
      return $23;
    }
    function toJson(o) {
      var $24 = JSON.stringify;
      var $25 = $24(o);
      return $25;
    }
    function parseJson(s) {
      var $26 = JSON.parse;
      var $27 = $26(s);
      return $27;
    }
    function Compiler(a0) {
      return ["Compiler", a0];
    }
    function newCompiler(onresult) {
      var $28 = Compiler({lastCompiledOptimize:$builtin_False, xhr:$builtin_None, lastCompiledSource:$builtin_None, timerId:$builtin_None, onresult:onresult});
      return $28;
    }
    function compile(compiler, source, optimize) {
      {
        var this$ = compiler[1];
      }
      var $29 = (this$).timerId;
      var $30;
      if (("Some"===$29[0])) {
        {
          var tid = $29[1];
        }
        var $31 = clearTimeout(tid);
        $30 = $31;
      }
      else {
        if (("None"===$29[0])) {
          {
          }
          $30 = (void 0);
        }
        else {
        }
      }
      var $32 = (this$).xhr;
      var $33;
      if (("Some"===$32[0])) {
        {
          var x = $32[1];
        }
        var $34 = (x).abort();
        (this$).xhr = $builtin_None;
        $33 = (void 0);
      }
      else {
        if (("None"===$32[0])) {
          {
          }
          $33 = (void 0);
        }
        else {
        }
      }
      var $35 = (this$).lastCompiledSource;
      var $36;
      if (("Some"===$35[0])) {
        {
          var lcs = $35[1];
        }
        var $37 = (lcs===source);
        var $38 = (this$).lastCompiledOptimize;
        var $39 = (optimize===$38);
        var $40 = ($37&&$39);
        var $41;
        if ($40) {
          return (void 0);
        }
        else {
          $41 = (void 0);
        }
        $36 = $41;
      }
      else {
        {
        }
        $36 = (void 0);
      }
      var $70 = setTimeout((function() {
        (this$).timerId = $builtin_None;
        var $42 = compileCrux(source);
        var $43;
        if (("Err"===$42[0])) {
          {
            var error = $42[1];
          }
          var $44 = $builtin_Some(source);
          (this$).lastCompiledSource = $44;
          var $45 = ("Compile error:\n"+error);
          var $46 = Err($45);
          var $47 = (this$).onresult($46);
          return (void 0);
        }
        else {
          if (("Ok"===$42[0])) {
            {
              var res = $42[1];
            }
            var $48 = $builtin_not(optimize);
            var $49;
            if ($48) {
              var $50 = $builtin_Some(source);
              (this$).lastCompiledSource = $50;
              var $51 = Ok(res);
              var $52 = (this$).onresult($51);
              return (void 0);
            }
            else {
              $49 = res;
            }
            $43 = $49;
          }
          else {
          }
        }
        {
          var result = $43;
        }
        var $53 = newXmlHttpRequest();
        {
          var xhr = $53;
        }
        var $54 = $builtin_Some(xhr);
        (this$).xhr = $54;
        var $55 = (xhr).open("POST", "https://crux-closure-service.herokuapp.com/compile");
        var $56 = (xhr).setRequestHeader("content-type", "application/json");
        (xhr).timeout = 15000;
        var $57 = toJson({source:result});
        var $58 = (xhr).send($57);
        (xhr).onload = (function() {
          var $59 = (xhr).response;
          var $60 = parseJson($59);
          {
            var result = $60;
          }
          (this$).xhr = $builtin_None;
          var $61 = $builtin_Some(source);
          (this$).lastCompiledSource = $61;
          var $62 = (result).source;
          var $63 = Ok($62);
          var $64 = (this$).onresult($63);
          return $64;
        });
        (xhr).onerror = (function(e) {
          (this$).xhr = $builtin_None;
          (this$).lastCompiledSource = $builtin_None;
          var $65 = ("Network error:\n"+e);
          var $66 = Err($65);
          var $67 = (this$).onresult($66);
          return $67;
        });
        (xhr).ontimeout = (function() {
          (this$).xhr = $builtin_None;
          (this$).lastCompiledSource = $builtin_None;
          var $68 = Err("Network timeout");
          var $69 = (this$).onresult($68);
          return $69;
        });
        return (void 0);
      }), COMPILE_DELAY);
      var $71 = $builtin_Some($70);
      (this$).timerId = $71;
      return (void 0);
    }
    function main() {
      var $72 = querySelector(".crux-playground .source");
      {
        var sourceTextArea = $72;
      }
      var $73 = querySelector(".crux-playground .output");
      {
        var outputTextArea = $73;
      }
      var $74 = querySelector(".crux-playground .optimize");
      {
        var optimizeCheckbox = $74;
      }
      var $75 = querySelector(".crux-playground .run");
      {
        var runButton = $75;
      }
      {
        var loadExampleSource = (function() {
          var $76 = getElementById("initial_example");
          var $77 = ($76).text;
          {
            var source = $77;
          }
          (sourceTextArea).value = source;
          return (void 0);
        });
      }
      var $83 = newCompiler((function(result) {
        var $78;
        if (("Ok"===result[0])) {
          {
            var res = result[1];
          }
          var $79 = (outputTextArea).classList;
          var $80 = ($79).remove("has-errors");
          (outputTextArea).value = res;
          $78 = (void 0);
        }
        else {
          if (("Err"===result[0])) {
            {
              var err = result[1];
            }
            var $81 = (outputTextArea).classList;
            var $82 = ($81).add("has-errors");
            (outputTextArea).value = err;
            $78 = (void 0);
          }
          else {
          }
        }
        return $78;
      }));
      {
        var compiler = $83;
      }
      {
        var recompile = (function() {
          var $84 = (sourceTextArea).value;
          {
            var content = $84;
          }
          var $85 = (optimizeCheckbox).checked;
          {
            var optimize = $85;
          }
          var $86 = compile(compiler, content, optimize);
          return $86;
        });
      }
      {
        var registerCompileListener = (function() {
          var $87 = (sourceTextArea).addEventListener("input", recompile);
          var $88 = (optimizeCheckbox).addEventListener("change", recompile);
          return $88;
        });
      }
      var $89 = loadExampleSource();
      var $90 = registerCompileListener();
      var $91 = recompile();
      (sourceTextArea).disabled = $builtin_False;
      var $92 = (sourceTextArea).setSelectionRange(0, 0);
      var $93 = (sourceTextArea).focus();
      var $95 = (runButton).addEventListener("click", (function() {
        var $94 = $builtin_print("run button temporarily disabled");
        return $94;
      }));
      return $95;
    }
    var $96 = main();
  })();
})();
