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

public class Main {

  public static void main(String[] args) {
    port(Integer.valueOf(System.getenv("PORT")));
    //staticFileLocation("/public");

    get("/hello", (req, res) -> "Hello World");

    post("/compile", (request, response) -> {
        Compiler compiler = new Compiler();
        CompilerOptions options = new CompilerOptions();
        ArrayList<SourceFile> source = new ArrayList<>();
        source.add(SourceFile.fromCode("foo.js", "function foobar(x) {}"));
        Result result = compiler.compile(null, source, options);
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
