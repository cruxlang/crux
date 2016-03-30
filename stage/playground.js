(function() {
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
      var arr = $6;
      var i = 0;
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
      var i = 0;
      var $12 = len(arr);
      var length = $12;
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
      if (("True"==b[0])) {
        $0 = False;
      }
      else
        if (("False"==b[0])) {
          $0 = True;
        }
        else {
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
      var arr = $8;
      var i = 0;
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
      var i = 0;
      var $14 = len(arr);
      var length = $14;
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
      var h = $2;
      var $3 = needle;
      var n = $3;
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
      var h = $10;
      var $11 = needle;
      var n = $11;
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
      if ((("TimerId"==tid[0])&&true)) {
        var t = tid[1];
        $0 = t;
      }
      else {
      }
      return $0;
    }
    var COMPILE_DELAY = 1000;
    function as_bool(a) {
      var $1 = function (x) { return !!x; };
      var to_bool = $1;
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
      var rawRes = $15;
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
    function newCompiler(onresult) {
      return {lastCompiledOptimize:$builtin_False, xhr:$builtin_None, lastCompiledSource:$builtin_None, timerId:$builtin_None, onresult:onresult};
    }
    function compile(compiler, source, optimize) {
      var $28 = (compiler).timerId;
      var $29;
      if ((("Some"==$28[0])&&true)) {
        var tid = $28[1];
        var $30 = clearTimeout(tid);
        $29 = $30;
      }
      else
        if (("None"==$28[0])) {
          $29 = (void 0);
        }
        else {
        }
      var $31 = (compiler).xhr;
      var $32;
      if ((("Some"==$31[0])&&true)) {
        var x = $31[1];
        var $33 = (x).abort();
        (compiler).xhr = $builtin_None;
        $32 = (void 0);
      }
      else
        if (("None"==$31[0])) {
          $32 = (void 0);
        }
        else {
        }
      var $34 = (compiler).lastCompiledSource;
      var $35;
      if ((("Some"==$34[0])&&true)) {
        var lcs = $34[1];
        var $36 = (lcs===source);
        var $37 = (compiler).lastCompiledOptimize;
        var $38 = (optimize===$37);
        var $39 = ($36&&$38);
        var $40;
        if ($39) {
          return (void 0);
        }
        else {
          $40 = (void 0);
        }
        $35 = $40;
      }
      else
        if (true) {
          $35 = (void 0);
        }
        else {
        }
      var $69 = setTimeout((function() {
        (compiler).timerId = $builtin_None;
        var $41 = compileCrux(source);
        var $42;
        if ((("Err"==$41[0])&&true)) {
          var error = $41[1];
          var $43 = $builtin_Some(source);
          (compiler).lastCompiledSource = $43;
          var $44 = ("Compile error:\n"+error);
          var $45 = Err($44);
          var $46 = (compiler).onresult($45);
          return (void 0);
        }
        else
          if ((("Ok"==$41[0])&&true)) {
            var res = $41[1];
            var $47 = $builtin_not(optimize);
            var $48;
            if ($47) {
              var $49 = $builtin_Some(source);
              (compiler).lastCompiledSource = $49;
              var $50 = Ok(res);
              var $51 = (compiler).onresult($50);
              return (void 0);
            }
            else {
              $48 = res;
            }
            $42 = $48;
          }
          else {
          }
        var result = $42;
        var $52 = newXmlHttpRequest();
        var xhr = $52;
        var $53 = $builtin_Some(xhr);
        (compiler).xhr = $53;
        var $54 = (xhr).open("POST", "https://crux-closure-service.herokuapp.com/compile");
        var $55 = (xhr).setRequestHeader("content-type", "application/json");
        (xhr).timeout = 15000;
        var $56 = toJson({source:result});
        var $57 = (xhr).send($56);
        (xhr).onload = (function() {
          var $58 = (xhr).response;
          var $59 = parseJson($58);
          var result = $59;
          (compiler).xhr = $builtin_None;
          var $60 = $builtin_Some(source);
          (compiler).lastCompiledSource = $60;
          var $61 = (result).source;
          var $62 = Ok($61);
          var $63 = (compiler).onresult($62);
          return $63;
        });
        (xhr).onerror = (function(e) {
          (compiler).xhr = $builtin_None;
          (compiler).lastCompiledSource = $builtin_None;
          var $64 = ("Network error:\n"+e);
          var $65 = Err($64);
          var $66 = (compiler).onresult($65);
          return $66;
        });
        (xhr).ontimeout = (function() {
          (compiler).xhr = $builtin_None;
          (compiler).lastCompiledSource = $builtin_None;
          var $67 = Err("Network timeout");
          var $68 = (compiler).onresult($67);
          return $68;
        });
        return (void 0);
      }), COMPILE_DELAY);
      var $70 = $builtin_Some($69);
      (compiler).timerId = $70;
      return (void 0);
    }
    function main() {
      var $71 = querySelector(".crux-playground .source");
      var sourceTextArea = $71;
      var $72 = querySelector(".crux-playground .output");
      var outputTextArea = $72;
      var $73 = querySelector(".crux-playground .optimize");
      var optimizeCheckbox = $73;
      var $74 = querySelector(".crux-playground .run");
      var runButton = $74;
      var loadExampleSource = (function() {
        var $75 = getElementById("initial_example");
        var $76 = ($75).text;
        var source = $76;
        (sourceTextArea).value = source;
        return (void 0);
      });
      var $82 = newCompiler((function(result) {
        var $77;
        if ((("Ok"==result[0])&&true)) {
          var res = result[1];
          var $78 = (outputTextArea).classList;
          var $79 = ($78).remove("has-errors");
          (outputTextArea).value = res;
          $77 = (void 0);
        }
        else
          if ((("Err"==result[0])&&true)) {
            var err = result[1];
            var $80 = (outputTextArea).classList;
            var $81 = ($80).add("has-errors");
            (outputTextArea).value = err;
            $77 = (void 0);
          }
          else {
          }
        return $77;
      }));
      var compiler = $82;
      var recompile = (function() {
        var $83 = (sourceTextArea).value;
        var content = $83;
        var $84 = (optimizeCheckbox).checked;
        var optimize = $84;
        var $85 = compile(compiler, content, optimize);
        return $85;
      });
      var registerCompileListener = (function() {
        var $86 = (sourceTextArea).addEventListener("input", recompile);
        var $87 = (optimizeCheckbox).addEventListener("change", recompile);
        return $87;
      });
      var $88 = loadExampleSource();
      var $89 = registerCompileListener();
      var $90 = recompile();
      (sourceTextArea).disabled = $builtin_False;
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
