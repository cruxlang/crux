function compileCrux(source) {
    var ret = {};
    window.hs_compileCrux(source, ret);
    return ret;
}

function main() {
    var COMPILE_DELAY = 1000; // milliseconds
    
    var sourceTextArea = document.querySelector('.crux-playground .source');
    var outputTextArea = document.querySelector('.crux-playground .output');
    
    var runButton = document.querySelector('.crux-playground .run');

    function loadExampleSource() {
        // load the initial source text
        var data = document.getElementById('initial_example').text;
        data = data.replace(/^\s+/, ''); // trim leading whitespace
        sourceTextArea.value = data;
    }

    function recompile() {
        var content = sourceTextArea.value;
        var res = compileCrux(content);
        if (res.error) {
            outputTextArea.classList.add("has-errors");
            outputTextArea.value = res.error;
        } else {
            outputTextArea.classList.remove("has-errors");
            outputTextArea.value = res.result;
        }
    }

    function registerCompileListener() {
        sourceTextArea.addEventListener(
            'input',
            _.debounce(recompile, COMPILE_DELAY));
    }

    loadExampleSource();
    registerCompileListener();
    recompile();

    sourceTextArea.disabled = false;
    sourceTextArea.setSelectionRange(0, 0);
    sourceTextArea.focus();

    runButton.addEventListener('click', function () {
        var content = sourceTextArea.value;
        var res = compileCrux(content);
        if (res.error) {
            alert("Compile failed. See errors");
        } else {
            eval(res.result);
        }
    });
}

window.addEventListener("message", function onmessage(message) {
    if (message.source === window && message.data === "crux-playground-loaded") {
        main();
    }
});
