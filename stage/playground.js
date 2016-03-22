// TODO: rewrite this in Crux

(function() {
  var COMPILE_DELAY = 1000; // milliseconds

  function compileCrux(source) {
    return window.compileCrux(source);
  }

  function Compiler(onresult) {
    this.onresult = onresult;
      
    this.lastCompiledSource = null;
    this.lastCompiledOptimize = null;
    
    this.lastCompiledResult = null;
    this.lastCompiledError = null;

    this.timerId = null;
    this.xhr = null;
  }

  Compiler.prototype.getLastCompileResult = function getLastCompileResult() {
    return this.lastCompiledResult;
  };

  Compiler.prototype.getLastCompileError = function getLastCompileError() {
    return this.lastCompiledError;
  };

  Compiler.prototype.compile = function compile(source, optimize) {
    // if timer is live, stop timer
    // if xhr is live, abort xhr
    // if source matches last successful compilation, do nothing
    // start timer
    //   when timer triggers, compile source with crux, then xhr to closure
    // if timer is live, restart timer
    // if xhr is live, abort xhr, if compiled more than last second, start closure compilation
    // if doing closure compilation, abort, then reschedule

    if (this.timerId !== null) {
      window.clearTimeout(this.timerId);
      this.timerId = null;
    }

    if (this.xhr !== null) {
      this.xhr.abort();
      this.xhr = null;
    }

    if (source === this.lastCompiledSource && optimize === this.lastCompiledOptimize) {
      return;
    }

    this.timerId = window.setTimeout(function() {
      this.timerId = null;

      var res = compileCrux(source);
      if (res.error) {
        this.lastCompiledSource = source;
        this.lastCompiledResult = null;
        this.lastCompiledError = "Compile error:\n" + res.error;
        this.onresult();
        return;
      }

      if (!optimize) {
        this.lastCompiledSource = source;
        this.lastCompiledResult = res.result;
        this.lastCompiledError = null;
        this.onresult();
        return;
      }

      this.xhr = new XMLHttpRequest;
      this.xhr.open('POST', 'https://crux-closure-service.herokuapp.com/compile');
      this.xhr.setRequestHeader('content-type', 'application/json');
      this.xhr.timeout = 15000; // 15s
      this.xhr.send(JSON.stringify({source: res.result}));
      
      this.xhr.onload = function onload() {
        var result = JSON.parse(this.xhr.responseText);
        this.xhr = null;
        this.lastCompiledSource = source;
        this.lastCompiledResult = result.source;
        this.lastCompiledError = null;
        this.onresult();
      }.bind(this);
      this.xhr.onerror = function onerror(e) {
        this.xhr = null;
        this.lastCompiledSource = null;
        this.lastCompiledResult = null;
        this.lastCompiledError = "Network error:\n" + e;
        this.onresult();
      }.bind(this);
      this.xhr.ontimeout = function() {
        this.xhr = null;
        this.lastCompiledSource = null;
        this.lastCompiledResult = null;
        this.lastCompiledError = "Network timeout";
        this.onresult();
      }.bind(this);
    }.bind(this), COMPILE_DELAY); // 1s
  };

  function main() {
    var sourceTextArea = document.querySelector('.crux-playground .source');
    var outputTextArea = document.querySelector('.crux-playground .output');

    var optimizeCheckbox = document.querySelector('.crux-playground .optimize');
    var runButton = document.querySelector('.crux-playground .run');

    function loadExampleSource() {
      // load the initial source text
      var data = document.getElementById('initial_example').text;
      data = data.replace(/^\s+/, ''); // trim leading whitespace
      sourceTextArea.value = data;
    }

    var compiler = new Compiler(function() {
      var error = compiler.getLastCompileError();
      if (error) {
        outputTextArea.classList.add("has-errors");
        outputTextArea.value = error;
      } else {
        outputTextArea.classList.remove("has-errors");
        outputTextArea.value = compiler.getLastCompileResult();
      }
    });
    
    function recompile() {
      var content = sourceTextArea.value;
      var optimize = optimizeCheckbox.checked;
      compiler.compile(content, optimize);
    }

    function registerCompileListener() {
      sourceTextArea.addEventListener('input', recompile);
      optimizeCheckbox.addEventListener('change', recompile);
    }

    loadExampleSource();
    registerCompileListener();
    recompile();

    sourceTextArea.disabled = false;
    sourceTextArea.setSelectionRange(0, 0);
    sourceTextArea.focus();

    runButton.addEventListener('click', function () {
      alert('functionality temporarily disabled');
      /*
      var content = sourceTextArea.value;
      var res = compileCrux(content);
      if (res.error) {
        alert("Compile failed. See errors");
      } else {
        eval(res.result);
      }
      */
    });
  }

  main();
})();
