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
  var $types_True;
  var $types_False;
  var $types_Void;
  (function() {
    var True = true;
    var False = false;
    var Void = (void 0);
    $types_True = True;
    $types_False = False;
    $types_Void = Void;
  })();
  var $js$unsafe_not;
  var $js$unsafe_eq;
  var $js$unsafe_neq;
  var $js$unsafe_lt;
  var $js$unsafe_lte;
  var $js$unsafe_gt;
  var $js$unsafe_gte;
  (function() {
    var $0 = function not(b) { return !b; };
    var not = $0;
    var $1 = function eq(lhs, rhs) { return lhs === rhs; };
    var eq = $1;
    var $2 = function neq(lhs, rhs) { return lhs !== rhs; };
    var neq = $2;
    var $3 = function lt(lhs, rhs) { return lhs < rhs; };
    var lt = $3;
    var $4 = function lte(lhs, rhs) { return lhs <= rhs; };
    var lte = $4;
    var $5 = function gt(lhs, rhs) { return lhs > rhs; };
    var gt = $5;
    var $6 = function gte(lhs, rhs) { return lhs >= rhs; };
    var gte = $6;
    $js$unsafe_not = not;
    $js$unsafe_eq = eq;
    $js$unsafe_neq = neq;
    $js$unsafe_lt = lt;
    $js$unsafe_lte = lte;
    $js$unsafe_gt = gt;
    $js$unsafe_gte = gte;
  })();
  var $boolean_not;
  (function() {
    var not = $js$unsafe_not;
    $boolean_not = not;
  })();
  var $cmp_eq;
  var $cmp_neq;
  var $$Boolean$Eq$$$types$$$cmp;
  var $cmp_lt;
  var $cmp_gte;
  var $cmp_lte;
  var $cmp_gt;
  var $cmp_min;
  var $cmp_max;
  var $cmp_LessThan;
  var $cmp_EqualTo;
  var $cmp_GreaterThan;
  var $$Ordering$Eq$$$cmp$$$cmp;
  var $cmp_compare;
  (function() {
    function eq(dict) {
      return (dict).eq;
    }
    function neq($cmp$Eq$13) {
      return (function(lhs, rhs) {
        var $0 = eq($cmp$Eq$13);
        var $1 = $0(lhs, rhs);
        var $2 = $boolean_not($1);
        return $2;
      });
    }
    $$Boolean$Eq$$$types$$$cmp = {eq:$js$unsafe_eq};
    function lt(dict) {
      return (dict).lt;
    }
    function gte($cmp$Ordered$28) {
      return (function(lhs, rhs) {
        var $3 = lt($cmp$Ordered$28);
        var $4 = $3(lhs, rhs);
        var $5 = $boolean_not($4);
        return $5;
      });
    }
    function lte($cmp$Eq$42, $cmp$Ordered$42) {
      return (function(lhs, rhs) {
        var $6 = lt($cmp$Ordered$42);
        var $7 = $6(lhs, rhs);
        var $8 = eq($cmp$Eq$42);
        var $9 = $8(lhs, rhs);
        var $10 = ($7||$9);
        return $10;
      });
    }
    function gt($cmp$Eq$56, $cmp$Ordered$56) {
      return (function(lhs, rhs) {
        var $11 = lt($cmp$Ordered$56);
        var $12 = $11(rhs, lhs);
        var $13 = neq($cmp$Eq$56);
        var $14 = $13(lhs, rhs);
        var $15 = ($12&&$14);
        return $15;
      });
    }
    function min($cmp$Ordered$62) {
      return (function(lhs, rhs) {
        var $16 = lt($cmp$Ordered$62);
        var $17 = $16(lhs, rhs);
        var $18;
        if ($17) {
          $18 = lhs;
        }
        else {
          $18 = rhs;
        }
        return $18;
      });
    }
    function max($cmp$Ordered$74) {
      return (function(lhs, rhs) {
        var $19 = lt($cmp$Ordered$74);
        var $20 = $19(rhs, lhs);
        var $21;
        if ($20) {
          $21 = lhs;
        }
        else {
          $21 = rhs;
        }
        return $21;
      });
    }
    var LessThan = "negative-one-todo";
    var EqualTo = 0;
    var GreaterThan = 1;
    $$Ordering$Eq$$$cmp$$$cmp = {eq:$js$unsafe_eq};
    function compare($cmp$Ordered$100) {
      return (function(lhs, rhs) {
        var $22 = lt($cmp$Ordered$100);
        var $23 = $22(lhs, rhs);
        var $24;
        if ($23) {
          $24 = LessThan;
        }
        else {
          var $25 = lt($cmp$Ordered$100);
          var $26 = $25(rhs, lhs);
          var $27;
          if ($26) {
            $27 = GreaterThan;
          }
          else {
            $27 = EqualTo;
          }
          $24 = $27;
        }
        return $24;
      });
    }
    $cmp_eq = eq;
    $cmp_neq = neq;
    $$Boolean$Eq$$$types$$$cmp = $$Boolean$Eq$$$types$$$cmp;
    $cmp_lt = lt;
    $cmp_gte = gte;
    $cmp_lte = lte;
    $cmp_gt = gt;
    $cmp_min = min;
    $cmp_max = max;
    $cmp_LessThan = LessThan;
    $cmp_EqualTo = EqualTo;
    $cmp_GreaterThan = GreaterThan;
    $$Ordering$Eq$$$cmp$$$cmp = $$Ordering$Eq$$$cmp$$$cmp;
    $cmp_compare = compare;
  })();
  var $$Number$Eq$$$number$$$cmp;
  var $$Number$Ordered$$$number$$$cmp;
  (function() {
    $$Number$Eq$$$number$$$cmp = {eq:$js$unsafe_eq};
    $$Number$Ordered$$$number$$$cmp = {lt:$js$unsafe_lt};
    $$Number$Eq$$$number$$$cmp = $$Number$Eq$$$number$$$cmp;
    $$Number$Ordered$$$number$$$cmp = $$Number$Ordered$$$number$$$cmp;
  })();
  var $length_len;
  (function() {
    function len(dict) {
      return (dict).len;
    }
    $length_len = len;
  })();
  var $array_get;
  var $$Array$HasLength$$$array$$$length;
  var $$Array$Eq$$$array$$$cmp;
  var $array_replicate;
  var $array_each;
  var $array_sliceFrom;
  var $array_sliceTo;
  var $array_slice;
  var $array_map;
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
    $$Array$HasLength$$$array$$$length = {len:(function(a) {
      var $4 = a;
      var $5 = ($4).length;
      return $5;
    })};
    $$Array$Eq$$$array$$$cmp = (function($cmp$Eq$42) {
      return {eq:(function(lhs, rhs) {
        var $6 = $length_len($$Array$HasLength$$$array$$$length);
        var $7 = $6(lhs);
        {
          var lhs_length = $7;
        }
        var $8 = $length_len($$Array$HasLength$$$array$$$length);
        var $9 = $8(rhs);
        {
          var rhs_length = $9;
        }
        var $10 = $cmp_neq($$Number$Eq$$$number$$$cmp);
        var $11 = $10(lhs_length, rhs_length);
        var $12;
        if ($11) {
          return $types_False;
        }
        else {
          $12 = (void 0);
        }
        {
          var i = 0;
        }
        while (true)
        {
          var $13 = $cmp_lt($$Number$Ordered$$$number$$$cmp);
          var $14 = $13(i, lhs_length);
          var $15 = (!$14);
          var $16;
          if ($15) {
            break;
          }
          else {
            $16 = (void 0);
          }
          var $17 = $cmp_neq($cmp$Eq$42);
          var $18 = get(lhs, i);
          var $19 = get(rhs, i);
          var $20 = $17($18, $19);
          var $21;
          if ($20) {
            return $types_False;
          }
          else {
            $21 = (void 0);
          }
          var $22 = (i+1);
          i = $22;
        }
        return $types_True;
      })};
    });
    function replicate(element, n) {
      var $23 = _unsafe_new(n);
      {
        var arr = $23;
      }
      {
        var i = 0;
      }
      while (true)
      {
        var $24 = $cmp_lt($$Number$Ordered$$$number$$$cmp);
        var $25 = $24(i, n);
        var $26 = (!$25);
        var $27;
        if ($26) {
          break;
        }
        else {
          $27 = (void 0);
        }
        var $28 = _unsafe_set(arr, i, element);
        var $29 = (i+1);
        i = $29;
      }
      return arr;
    }
    function each(arr, f) {
      {
        var i = 0;
      }
      var $30 = $length_len($$Array$HasLength$$$array$$$length);
      var $31 = $30(arr);
      {
        var n = $31;
      }
      while (true)
      {
        var $32 = $cmp_lt($$Number$Ordered$$$number$$$cmp);
        var $33 = $32(i, n);
        var $34 = (!$33);
        var $35;
        if ($34) {
          break;
        }
        else {
          $35 = (void 0);
        }
        var $36 = _unsafe_get(arr, i);
        var $37 = f($36);
        var $38 = (i+1);
        i = $38;
      }
      return (void 0);
    }
    function sliceFrom(arr, start) {
      var $39 = arr;
      var $40 = ($39).slice(start);
      return $40;
    }
    function sliceTo(arr, end) {
      var $41 = arr;
      var $42 = ($41).slice(0, end);
      return $42;
    }
    function slice(arr, start, end) {
      var $43 = arr;
      var $44 = ($43).slice(start, end);
      return $44;
    }
    function map(array, fn) {
      var $45 = array;
      var $46 = ($45).map(fn);
      return $46;
    }
    $array_get = get;
    $$Array$HasLength$$$array$$$length = $$Array$HasLength$$$array$$$length;
    $$Array$Eq$$$array$$$cmp = $$Array$Eq$$$array$$$cmp;
    $array_replicate = replicate;
    $array_each = each;
    $array_sliceFrom = sliceFrom;
    $array_sliceTo = sliceTo;
    $array_slice = slice;
    $array_map = map;
  })();
  var $option_Some;
  var $option_None;
  var $$Option$Eq$$$option$$$cmp;
  (function() {
    function Some(a0) {
      return ["Some", a0];
    }
    var None = ["None"];
    $$Option$Eq$$$option$$$cmp = (function($cmp$Eq$5) {
      return {eq:(function(lhs, rhs) {
        var $0 = $tuple_Tuple2(lhs, rhs);
        var $1;
        if (((("Tuple2"===$0[0])&&("None"===$0[1][0]))&&("None"===$0[2][0]))) {
          {
          }
          $1 = $types_True;
        }
        else {
          if (((("Tuple2"===$0[0])&&("Some"===$0[1][0]))&&("Some"===$0[2][0]))) {
            {
              var x = $0[1][1];
              var y = $0[2][1];
            }
            var $2 = $cmp_eq($cmp$Eq$5);
            var $3 = $2(x, y);
            $1 = $3;
          }
          else {
            {
            }
            $1 = $types_False;
          }
        }
        return $1;
      })};
    });
    $option_Some = Some;
    $option_None = None;
    $$Option$Eq$$$option$$$cmp = $$Option$Eq$$$option$$$cmp;
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
  var $$String$Eq$$$string$$$cmp;
  var $$String$HasLength$$$string$$$length;
  var $string_startsWith;
  var $string_endsWith;
  var $string_join;
  var $string_sliceFrom;
  var $string_trim;
  (function() {
    $$String$Eq$$$string$$$cmp = {eq:$js$unsafe_eq};
    $$String$HasLength$$$string$$$length = {len:(function(s) {
      var $0 = s;
      var $1 = ($0).length;
      return $1;
    })};
    function startsWith(haystack, needle) {
      var $2 = haystack;
      {
        var h = $2;
      }
      var $3 = needle;
      {
        var n = $3;
      }
      var $4 = $cmp_gt($$Number$Eq$$$number$$$cmp, $$Number$Ordered$$$number$$$cmp);
      var $5 = (n).length;
      var $6 = (h).length;
      var $7 = $4($5, $6);
      var $8;
      if ($7) {
        return $types_False;
      }
      else {
        $8 = (void 0);
      }
      var $9 = $cmp_eq($$Number$Eq$$$number$$$cmp);
      var $10 = (h).indexOf(needle, 0);
      var $11 = $9($10, 0);
      return $11;
    }
    function endsWith(haystack, needle) {
      var $12 = haystack;
      {
        var h = $12;
      }
      var $13 = needle;
      {
        var n = $13;
      }
      var $14 = $cmp_gt($$Number$Eq$$$number$$$cmp, $$Number$Ordered$$$number$$$cmp);
      var $15 = (n).length;
      var $16 = (h).length;
      var $17 = $14($15, $16);
      var $18;
      if ($17) {
        return $types_False;
      }
      else {
        $18 = (void 0);
      }
      var $19 = $cmp_neq($$Number$Eq$$$number$$$cmp);
      var $20 = (h).length;
      var $21 = (n).length;
      var $22 = ($20-$21);
      var $23 = (h).indexOf(needle, $22);
      var $24 = (0-1);
      var $25 = $19($23, $24);
      return $25;
    }
    function join(sep, elements) {
      var $26 = elements;
      var $27 = ($26).join(sep);
      return $27;
    }
    function sliceFrom(s, start) {
      var $28 = s;
      var $29 = ($28).slice(start);
      return $29;
    }
    function trim(s) {
      var $30 = s;
      var $31 = ($30).trim();
      return $31;
    }
    $$String$Eq$$$string$$$cmp = $$String$Eq$$$string$$$cmp;
    $$String$HasLength$$$string$$$length = $$String$HasLength$$$string$$$length;
    $string_startsWith = startsWith;
    $string_endsWith = endsWith;
    $string_join = join;
    $string_sliceFrom = sliceFrom;
    $string_trim = trim;
  })();
  var $mutarray_append;
  var $mutarray_get;
  var $$MutableArray$HasLength$$$mutarray$$$length;
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
    $$MutableArray$HasLength$$$mutarray$$$length = {len:(function(arr) {
      var $6 = arr;
      var $7 = ($6).length;
      return $7;
    })};
    function replicate(element, n) {
      var $8 = _unsafe_new(n);
      {
        var arr = $8;
      }
      {
        var i = 0;
      }
      while (true)
      {
        var $9 = $cmp_lt($$Number$Ordered$$$number$$$cmp);
        var $10 = $9(i, n);
        var $11 = (!$10);
        var $12;
        if ($11) {
          break;
        }
        else {
          $12 = (void 0);
        }
        var $13 = _unsafe_set(arr, i, element);
        var $14 = (i+1);
        i = $14;
      }
      return arr;
    }
    function each(arr, f) {
      {
        var i = 0;
      }
      var $15 = $length_len($$MutableArray$HasLength$$$mutarray$$$length);
      var $16 = $15(arr);
      {
        var n = $16;
      }
      while (true)
      {
        var $17 = $cmp_lt($$Number$Ordered$$$number$$$cmp);
        var $18 = $17(i, n);
        var $19 = (!$18);
        var $20;
        if ($19) {
          break;
        }
        else {
          $20 = (void 0);
        }
        var $21 = _unsafe_get(arr, i);
        var $22 = f($21);
        var $23 = (i+1);
        i = $23;
      }
      return (void 0);
    }
    function sliceFrom(arr, start) {
      var $24 = arr;
      var $25 = ($24).slice(start);
      return $25;
    }
    function sliceTo(arr, end) {
      var $26 = arr;
      var $27 = ($26).slice(0, end);
      return $27;
    }
    function slice(arr, start, end) {
      var $28 = arr;
      var $29 = ($28).slice(start, end);
      return $29;
    }
    function freeze(arr) {
      var $30 = arr;
      var $31 = ($30).slice();
      return $31;
    }
    function sort(arr) {
      var $32 = arr;
      var $33 = ($32).sort();
      return $33;
    }
    $mutarray_append = append;
    $mutarray_get = get;
    $$MutableArray$HasLength$$$mutarray$$$length = $$MutableArray$HasLength$$$mutarray$$$length;
    $mutarray_replicate = replicate;
    $mutarray_each = each;
    $mutarray_sliceFrom = sliceFrom;
    $mutarray_sliceTo = sliceTo;
    $mutarray_slice = slice;
    $mutarray_freeze = freeze;
    $mutarray_sort = sort;
  })();
  var $tuple_Tuple2;
  var $tuple_Tuple3;
  var $tuple_Tuple4;
  var $tuple_Tuple5;
  var $tuple_Tuple6;
  var $tuple_Tuple7;
  var $tuple_Tuple8;
  (function() {
    function Tuple2(a0, a1) {
      return ["Tuple2", a0, a1];
    }
    function Tuple3(a0, a1, a2) {
      return ["Tuple3", a0, a1, a2];
    }
    function Tuple4(a0, a1, a2, a3) {
      return ["Tuple4", a0, a1, a2, a3];
    }
    function Tuple5(a0, a1, a2, a3, a4) {
      return ["Tuple5", a0, a1, a2, a3, a4];
    }
    function Tuple6(a0, a1, a2, a3, a4, a5) {
      return ["Tuple6", a0, a1, a2, a3, a4, a5];
    }
    function Tuple7(a0, a1, a2, a3, a4, a5, a6) {
      return ["Tuple7", a0, a1, a2, a3, a4, a5, a6];
    }
    function Tuple8(a0, a1, a2, a3, a4, a5, a6, a7) {
      return ["Tuple8", a0, a1, a2, a3, a4, a5, a6, a7];
    }
    $tuple_Tuple2 = Tuple2;
    $tuple_Tuple3 = Tuple3;
    $tuple_Tuple4 = Tuple4;
    $tuple_Tuple5 = Tuple5;
    $tuple_Tuple6 = Tuple6;
    $tuple_Tuple7 = Tuple7;
    $tuple_Tuple8 = Tuple8;
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
  var $$LastCompile$Eq$$$main$$$cmp;
  (function() {
    function TimerId(a0) {
      return ["TimerId", a0];
    }
    function unTimerId($_0) {
      var t = $_0[1];
      return t;
    }
    var COMPILE_DELAY = 1000;
    function unsafe_as_bool(a) {
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
      var $16 = unsafe_as_bool($15);
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
    $$LastCompile$Eq$$$main$$$cmp = {eq:(function($_1, $_2) {
      var a = $_1[1];
      var b = $_1[2];
      var c = $_2[1];
      var d = $_2[2];
      var $27 = $cmp_eq($$String$Eq$$$string$$$cmp);
      var $28 = $27(a, c);
      var $29 = $cmp_eq($$Boolean$Eq$$$types$$$cmp);
      var $30 = $29(b, d);
      var $31 = ($28&&$30);
      return $31;
    })};
    function Compiler(a0) {
      return ["Compiler", a0];
    }
    function newCompiler(onresult) {
      var $32 = Compiler({state:Idle, lastCompile:$option_None, onresult:onresult});
      return $32;
    }
    function compile($_3, source, optimize) {
      var this$ = $_3[1];
      var $33 = (this$).state;
      var $34;
      if (("Idle"===$33[0])) {
        {
        }
        $34 = (void 0);
      }
      else {
        if (("Waiting"===$33[0])) {
          {
            var tid = $33[1];
          }
          var $35 = clearTimeout(tid);
          $34 = $35;
        }
        else {
          if (("Optimizing"===$33[0])) {
            {
              var xhr = $33[1];
            }
            var $36 = (xhr).abort();
            $34 = $36;
          }
          else {
          }
        }
      }
      (this$).state = Idle;
      var $37 = $$Option$Eq$$$option$$$cmp($$LastCompile$Eq$$$main$$$cmp);
      var $38 = $cmp_eq($37);
      var $39 = (this$).lastCompile;
      var $40 = LastCompile(source, optimize);
      var $41 = $option_Some($40);
      var $42 = $38($39, $41);
      var $43;
      if ($42) {
        return (void 0);
      }
      else {
        $43 = (void 0);
      }
      var $75 = setTimeout((function() {
        var $44 = compileCrux(source);
        var $45;
        if (("Err"===$44[0])) {
          {
            var error = $44[1];
          }
          (this$).state = Idle;
          var $46 = LastCompile(source, optimize);
          var $47 = $option_Some($46);
          (this$).lastCompile = $47;
          var $48 = ("Compile error:\n"+error);
          var $49 = $result_Err($48);
          var $50 = (this$).onresult($49);
          return (void 0);
        }
        else {
          if (("Ok"===$44[0])) {
            {
              var res = $44[1];
            }
            var $51 = $boolean_not(optimize);
            var $52;
            if ($51) {
              (this$).state = Idle;
              var $53 = LastCompile(source, optimize);
              var $54 = $option_Some($53);
              (this$).lastCompile = $54;
              var $55 = $result_Ok(res);
              var $56 = (this$).onresult($55);
              return (void 0);
            }
            else {
              $52 = res;
            }
            $45 = $52;
          }
          else {
          }
        }
        {
          var result = $45;
        }
        var $57 = newXmlHttpRequest();
        {
          var xhr = $57;
        }
        var $58 = (xhr).open("POST", "https://crux-closure-service.herokuapp.com/compile");
        var $59 = (xhr).setRequestHeader("content-type", "application/json");
        (xhr).timeout = 60000;
        var $60 = toJson({source:result});
        var $61 = (xhr).send($60);
        var $62 = Optimizing(xhr);
        (this$).state = $62;
        (xhr).onload = (function() {
          var $63 = (xhr).response;
          var $64 = parseJson($63);
          {
            var result2 = $64;
          }
          (this$).state = Idle;
          var $65 = LastCompile(source, optimize);
          var $66 = $option_Some($65);
          (this$).lastCompile = $66;
          var $67 = (result2).source;
          var $68 = $result_Ok($67);
          var $69 = (this$).onresult($68);
          return $69;
        });
        (xhr).onerror = (function(e) {
          (this$).lastCompile = $option_None;
          var $70 = ("Network error:\n"+e);
          var $71 = $result_Err($70);
          var $72 = (this$).onresult($71);
          return $72;
        });
        (xhr).ontimeout = (function() {
          (this$).lastCompile = $option_None;
          var $73 = $result_Err("Network timeout");
          var $74 = (this$).onresult($73);
          return $74;
        });
        return (void 0);
      }), COMPILE_DELAY);
      var $76 = Waiting($75);
      (this$).state = $76;
      return (void 0);
    }
    function main() {
      var $77 = querySelector(".crux-playground .source");
      {
        var sourceTextArea = $77;
      }
      var $78 = querySelector(".crux-playground .output");
      {
        var outputTextArea = $78;
      }
      var $79 = querySelector(".crux-playground .optimize");
      {
        var optimizeCheckbox = $79;
      }
      var $80 = querySelector(".crux-playground .run");
      {
        var runButton = $80;
      }
      {
        var loadExampleSource = (function() {
          var $81 = getElementById("initial_example");
          var $82 = ($81).text;
          {
            var source = $82;
          }
          var $83 = $string_trim(source);
          source = $83;
          (sourceTextArea).value = source;
          return (void 0);
        });
      }
      var $89 = newCompiler((function(result) {
        var $84;
        if (("Ok"===result[0])) {
          {
            var res = result[1];
          }
          var $85 = (outputTextArea).classList;
          var $86 = ($85).remove("has-errors");
          (outputTextArea).value = res;
          $84 = (void 0);
        }
        else {
          if (("Err"===result[0])) {
            {
              var err = result[1];
            }
            var $87 = (outputTextArea).classList;
            var $88 = ($87).add("has-errors");
            (outputTextArea).value = err;
            $84 = (void 0);
          }
          else {
          }
        }
        return $84;
      }));
      {
        var compiler = $89;
      }
      {
        var recompile = (function() {
          var $90 = (sourceTextArea).value;
          {
            var content = $90;
          }
          var $91 = (optimizeCheckbox).checked;
          {
            var optimize = $91;
          }
          var $92 = compile(compiler, content, optimize);
          return $92;
        });
      }
      {
        var registerCompileListener = (function() {
          var $93 = (sourceTextArea).addEventListener("input", recompile);
          var $94 = (optimizeCheckbox).addEventListener("change", recompile);
          return $94;
        });
      }
      var $95 = loadExampleSource();
      var $96 = registerCompileListener();
      var $97 = recompile();
      (sourceTextArea).disabled = $types_False;
      var $98 = (sourceTextArea).setSelectionRange(0, 0);
      var $99 = (sourceTextArea).focus();
      var $101 = (runButton).addEventListener("click", (function() {
        var $100 = $builtin_print("run button temporarily disabled");
        return $100;
      }));
      return $101;
    }
    var $102 = main();
    $$LastCompile$Eq$$$main$$$cmp = $$LastCompile$Eq$$$main$$$cmp;
  })();
})();
