import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map;

import java.net.URI;
import java.net.URISyntaxException;

import static spark.Spark.*;
import spark.ResponseTransformer;
import spark.ModelAndView;

import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;
import com.google.javascript.jscomp.VariableRenamingPolicy;
import com.google.javascript.jscomp.PropertyRenamingPolicy;
import com.google.gson.Gson;

class CompileRequest {
    String filename;
    String source;
}

class CompileResponse {
    String source;
}

public class Main {
    public static ResponseTransformer json() {
        return (object) -> new Gson().toJson(object);
    }

    public static void main(String[] args) {
        port(Integer.valueOf(System.getenv("PORT")));
        //staticFileLocation("/public");

        get("/hello", (req, res) -> "Hello World");

        options("/compile", (req, res) -> {
            res.header("Access-Control-Allow-Origin", "*");
            String accessControlRequestHeaders = req.headers("Access-Control-Request-Headers");
            if (accessControlRequestHeaders != null) {
                res.header("Access-Control-Allow-Headers", accessControlRequestHeaders);
            }
            String accessControlRequestMethod = req.headers("Access-Control-Request-Method");
            if (accessControlRequestMethod != null) {
                res.header("Access-Control-Allow-Methods", accessControlRequestMethod);
            }
            return "OK";
        });

        post("/compile", (req, res) -> {
            String body = req.body();
            CompileRequest request = new Gson().fromJson(body, CompileRequest.class);
            if (null == request.filename) {
                request.filename = "<unspecified>.js";
            }

            Compiler compiler = new Compiler();
            CompilerOptions options = new CompilerOptions();
            CompilationLevel.SIMPLE_OPTIMIZATIONS.setOptionsForCompilationLevel(options);

            // This entire project exists because of the following line of code.  :|
            // See https://github.com/google/closure-compiler/issues/1668
            options.setRenamingPolicy(VariableRenamingPolicy.OFF, PropertyRenamingPolicy.OFF);
            options.setPrettyPrint(true);

            //List<SourceFile> builtinExterns = getBuiltinExterns(options.getEnvironment());
            ArrayList<SourceFile> externs = new ArrayList<>();
            ArrayList<SourceFile> source = new ArrayList<>();
            source.add(SourceFile.fromCode(request.filename, request.source));
            Result result = compiler.compile(externs, source, options);
            if (!result.success) {
                // result.warnings
                // result.errors
            }

            res.type("application/json");
            res.header("Access-Control-Allow-Origin", "*");

            CompileResponse response = new CompileResponse();
            response.source = compiler.toSource();
            return response;
            //return compiler.toSource();

            //response.status(200);
            //response.body(compiler.toSource());
        }, json());
    }
}
