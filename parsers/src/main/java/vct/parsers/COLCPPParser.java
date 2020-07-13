package vct.parsers;

import hre.lang.HREExitException;
import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import vct.antlr4.generated.PVLLexer;
import vct.antlr4.generated.PVLParser;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.syntax.PVLSyntax;
import vct.parsers.rewrite.FlattenVariableDeclarations;
import vct.parsers.rewrite.PVLPostProcessor;
import vct.parsers.rewrite.SpecificationCollector;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import static hre.lang.System.*;

public class COLCPPParser implements Parser {
    @Override
    public ProgramUnit parse(File file) {
        return null;
//        String file_name=file.toString();
//        try {
//            TimeKeeper tk=new TimeKeeper();
//            ErrorCounter ec=new ErrorCounter(file_name);
//
//            CharStream input = CharStreams.fromStream(new FileInputStream(file));
//            PVLLexer lexer = new PVLLexer(input);
//            lexer.removeErrorListeners();
//            lexer.addErrorListener(ec);
//            CommonTokenStream tokens = new CommonTokenStream(lexer);
//            PVLParser parser = new PVLParser(tokens);
//            parser.removeErrorListeners();
//            parser.addErrorListener(ec);
//            PVLParser.ProgramContext tree = parser.program();
//            Progress("parsing pass took %dms",tk.show());
//            ec.report();
//            Debug("parser got: %s",tree.toStringTree(parser));
//
//            ProgramUnit pu = PVLtoCOL.convert(tree,file_name,tokens,parser);
//            Progress("AST conversion pass took %dms",tk.show());
//
//            pu=new FlattenVariableDeclarations(pu).rewriteAll();
//            Progress("Variable pass took %dms",tk.show());
//
//            pu=new SpecificationCollector(PVLSyntax.get(),pu).rewriteAll();
//            Progress("Shuffling specifications took %dms",tk.show());
//            Debug("after collecting specifications %s",pu);
//
//            pu=new PVLPostProcessor(pu).rewriteAll();
//            Progress("Post processing pass took %dms",tk.show());
//            return pu;
//        } catch(HREExitException e) {
//            throw e;
//        } catch (FileNotFoundException e) {
//            Fail("File %s has not been found",file_name);
//        } catch (Exception e) {
//            DebugException(e);
//            Abort("Exception %s while parsing %s",e.getClass(),file_name);
//        } catch (Throwable e) {
//            DebugException(e);
//            Warning("Exception %s while parsing %s",e.getClass(),file_name);
//            throw e;
//        }
//        return null;
    }
}
