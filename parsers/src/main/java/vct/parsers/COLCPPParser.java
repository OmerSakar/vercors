package vct.parsers;

import hre.lang.HREExitException;
import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import vct.antlr4.generated.CPPParser;
import vct.antlr4.generated.LangCPPLexer;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.syntax.CSyntax;
import vct.parsers.rewrite.FlattenVariableDeclarations;
import vct.parsers.rewrite.SpecificationCollector;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import static hre.lang.System.*;

public class COLCPPParser implements Parser {
    @Override
    public ProgramUnit parse(File file) {
        String file_name=file.toString();
        try {
            TimeKeeper tk=new TimeKeeper();
            ErrorCounter ec=new ErrorCounter(file_name);

            CharStream input = CharStreams.fromStream(new FileInputStream(file));
            LangCPPLexer lexer = new LangCPPLexer(input);
            lexer.removeErrorListeners();
            lexer.addErrorListener(ec);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            CPPParser parser = new CPPParser(tokens);
            parser.removeErrorListeners();
            parser.addErrorListener(ec);
            CPPParser.TranslationunitContext tree = parser.translationunit();
            Progress("parsing pass took %dms",tk.show());
            ec.report();
            Debug("parser got: %s",tree.toStringTree(parser));

            ProgramUnit pu = CPPtoCOL.convert(tree,file_name,tokens,parser);
            Progress("AST conversion pass took %dms",tk.show());


//            Example from the PVLParser. It is a bit of a question what post-parse processing we need
            pu=new FlattenVariableDeclarations(pu).rewriteAll();
            Progress("Variable pass took %dms",tk.show());

            //TODO Create a CPPSyntax file
            pu=new SpecificationCollector(CSyntax.getCML(),pu).rewriteAll();
            Progress("Shuffling specifications took %dms",tk.show());
            Debug("after collecting specifications %s",pu);

            return pu;
        } catch(HREExitException e) {
            throw e;
        } catch (FileNotFoundException e) {
            Fail("File %s has not been found",file_name);
        } catch (Exception e) {
            DebugException(e);
	    StringBuilder message = new StringBuilder("\n");
            for (StackTraceElement s: e.getStackTrace()) { message.append(s.toString()).append("\n"); }
            Abort("Exception %s while parsing %s %s %s",e.getClass(),file_name, message.toString(), e.getMessage());
        } catch (Throwable e) {
            DebugException(e);
            Warning("Exception %s while parsing %s",e.getClass(),file_name);
            throw e;
        }
        return null;
    }
}
