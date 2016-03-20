import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map;

import java.net.URI;
import java.net.URISyntaxException;

import static spark.Spark.*;
import spark.ModelAndView;
import static spark.Spark.get;

import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;
import com.google.javascript.jscomp.VariableRenamingPolicy;
import com.google.javascript.jscomp.PropertyRenamingPolicy;

public class Main {

  public static void main(String[] args) {
    port(Integer.valueOf(System.getenv("PORT")));
    //staticFileLocation("/public");

    get("/hello", (req, res) -> "Hello World");

    post("/compile", (request, response) -> {
        Compiler compiler = new Compiler();
        CompilerOptions options = new CompilerOptions();
        CompilationLevel.SIMPLE_OPTIMIZATIONS.setOptionsForCompilationLevel(options);

        // This entire project exists because of the following line of code.  :|
        // See [TODO: find github issue]
        options.setRenamingPolicy(VariableRenamingPolicy.OFF, PropertyRenamingPolicy.OFF);
        options.prettyPrint = true;

        //List<SourceFile> builtinExterns = getBuiltinExterns(options.getEnvironment());
        ArrayList<SourceFile> externs = new ArrayList<>();
        ArrayList<SourceFile> source = new ArrayList<>();
        source.add(SourceFile.fromCode("foo.js", "function foobar(x) {}"));
        Result result = compiler.compile(externs, source, options);
        if (!result.success) {
            // result.warnings
            // result.errors
        }

        return compiler.toSource();

        //response.status(200);
        //response.body(compiler.toSource());
    });
  }

}
