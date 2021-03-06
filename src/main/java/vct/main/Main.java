// -*- tab-width:2 ; indent-tabs-mode:nil -*-

package vct.main;

import java.io.*;
import java.lang.reflect.Constructor;
import java.time.Instant;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.LinkedBlockingDeque;

import hre.ast.FileOrigin;
import hre.config.*;
import hre.lang.HREError;
import hre.lang.HREExitException;
import vct.col.util.LocalVariableChecker;
import hre.tools.TimeKeeper;
import vct.col.ast.util.AbstractRewriter;
import vct.col.rewrite.DeriveModifies;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.decl.SpecificationFormat;
import vct.col.ast.expr.StandardOperator;
import vct.col.rewrite.*;
import vct.col.rewrite.CheckHistoryAlgebra.Mode;
import vct.col.ast.syntax.JavaDialect;
import vct.col.ast.syntax.JavaSyntax;
import vct.col.ast.syntax.Syntax;
import vct.col.util.FeatureScanner;
import vct.col.util.JavaTypeCheck;
import vct.col.util.SimpleTypeCheck;
import vct.experiments.learn.SpecialCountVisitor;
import vct.experiments.learn.NonLinCountVisitor;
import vct.experiments.learn.Oracle;
import vct.logging.ErrorMapping;
import vct.logging.ExceptionMessage;
import vct.logging.PassReport;
import vct.parsers.rewrite.FlattenVariableDeclarations;
import vct.parsers.rewrite.InferADTTypes;
import vct.silver.ErrorDisplayVisitor;
import vct.test.CommandLineTesting;
import vct.col.ast.util.ClassName;
import hre.config.Configuration;

import static hre.lang.System.*;

/**
 * VerCors Tool main verifier.
 * @author Stefan Blom
 *
 */
public class Main
{
  private static ProgramUnit program=new ProgramUnit();

  private static List<ClassName> classes;
  
  private static Map<String, SpecialCountVisitor> counters = new HashMap<String, SpecialCountVisitor>();

  public static void main(String[] args) throws Throwable
  {
    int exit=0;
    long wallStart = System.currentTimeMillis();
    TimeKeeper tk = new TimeKeeper();
    try {
      hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info);
      hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info);

      OptionParser clops=new OptionParser();
      clops.add(clops.getHelpOption(),'h',"help");
      BooleanSetting version = new BooleanSetting(false);
      clops.add(version.getEnable("Output the current version and exit"), "version");

      ChoiceSetting logLevel = new ChoiceSetting(new String[] {"silent", "abort", "result", "warning", "info", "progress", "debug", "all"}, "info");
      clops.add(logLevel.getSetOption("Set the logging level"), "verbosity");
      clops.add(logLevel.getExplicitOption("progress", "Show progress through the passes"), "progress", 'v');
      clops.add(logLevel.getExplicitOption("silent", "Never output anything"), "silent", 'q');

      CollectSetting debugFilters = new CollectSetting();
      clops.add(debugFilters.getAddOption("Add a class to debug, or specify a line with Class:lineno"), "debug");

      BooleanSetting boogie=new BooleanSetting(false);
      clops.add(boogie.getEnable("select Boogie backend"),"boogie");
      BooleanSetting chalice=new BooleanSetting(false);
      clops.add(chalice.getEnable("select Chalice backend"),"chalice");
      final StringSetting silver=new StringSetting("silver");
      clops.add(silver.getAssign("select Silver backend (silicon/carbon)"),"silver");
      clops.add(silver.getAssign("select Silicon backend","silicon"),"silicon");
      clops.add(silver.getAssign("select Carbon backend","carbon"),"carbon");
      BooleanSetting dafny=new BooleanSetting(false);
      clops.add(dafny.getEnable("select Dafny backend"),"dafny");

      CommandLineTesting.addOptions(clops);

      final BooleanSetting check_defined=new BooleanSetting(false);
      clops.add(check_defined.getEnable("check if defined processes satisfy their contracts."),"check-defined");
      final BooleanSetting check_axioms=new BooleanSetting(false);
      clops.add(check_axioms.getEnable("check if defined processes satisfy their contracts."),"check-axioms");
      final BooleanSetting check_history=new BooleanSetting(false);
      clops.add(check_history.getEnable("check if defined processes satisfy their contracts."),"check-history");

      final BooleanSetting separate_checks=new BooleanSetting(false);
      clops.add(separate_checks.getEnable("validate classes separately"),"separate");
      BooleanSetting help_passes=new BooleanSetting(false);
      clops.add(help_passes.getEnable("print help on available passes"),"help-passes");
      BooleanSetting sequential_spec=new BooleanSetting(false);
      clops.add(sequential_spec.getEnable("sequential specification instead of concurrent"),"sequential");
      StringListSetting pass_list=new StringListSetting();
      Option pass_list_option=pass_list.getAppendOption("add to the custom list of compilation passes");
      clops.add(pass_list_option,"passes");
      StringListSetting show_before=new StringListSetting();
      clops.add(show_before.getAppendOption("Show source code before given passes"),"show-before");
      StringListSetting show_after=new StringListSetting();
      clops.add(show_after.getAppendOption("Show source code after given passes"),"show-after");
      StringSetting show_file=new StringSetting(null);
      clops.add(show_file.getAssign("redirect show output to files instead of stdout"),"save-show");
      CollectSetting debugBefore = new CollectSetting();
      CollectSetting debugAfter = new CollectSetting();
      clops.add(debugBefore.getAddOption("Dump the COL AST before a pass is run"), "debug-before");
      clops.add(debugAfter.getAddOption("Dump the COL AST after a pass is run"), "debug-after");
      StringListSetting stop_after=new StringListSetting();
      clops.add(stop_after.getAppendOption("Stop after given passes"),"stop-after");

      BooleanSetting abruptTerminationViaExceptions = new BooleanSetting(false);
      clops.add(abruptTerminationViaExceptions.getEnable("Force compilation of abrupt termination to exceptions"), "at-via-exceptions");


      BooleanSetting explicit_encoding=new BooleanSetting(false);
      clops.add(explicit_encoding.getEnable("explicit encoding"),"explicit");

      clops.add_removed("the inline option was removed in favor of the inline modifer","inline");

      BooleanSetting global_with_field=new BooleanSetting(false);
      clops.add(global_with_field.getEnable("Encode global access with a field rather than a parameter. (expert option)"),"global-with-field");

      BooleanSetting infer_modifies=new BooleanSetting(false);
      clops.add(infer_modifies.getEnable("infer modifies clauses"),"infer-modifies");
      BooleanSetting no_context=new BooleanSetting(false);
      clops.add(no_context.getEnable("disable printing the context of errors"),"no-context");
      BooleanSetting gui_context=new BooleanSetting(false);
      clops.add(gui_context.getEnable("enable the gui extension of the context"),"gui");

      BooleanSetting sat_check=new BooleanSetting(true);
      clops.add(sat_check.getDisable("Disable checking if method pre-conditions are satisfiable"), "disable-sat");

      IntegerSetting trigger_generation = new IntegerSetting(0);
      clops.add(trigger_generation.getOptionalAssign("Try to simplify universal quantifiers and generate triggers for them."), "triggers");
      
      BooleanSetting learn = new BooleanSetting(false);
      clops.add(learn.getEnable("Learn unit times for AST nodes."), "learn");
      
      Configuration.add_options(clops);

      String input[]=clops.parse(args);

      hre.lang.System.LogLevel level = hre.lang.System.LogLevel.Info;

      switch(logLevel.get()) {
        case "silent":
          level = hre.lang.System.LogLevel.Silent;
          break;
        case "abort":
          level = hre.lang.System.LogLevel.Abort;
          break;
        case "result":
          level = hre.lang.System.LogLevel.Result;
          break;
        case "warning":
          level = hre.lang.System.LogLevel.Warning;
          break;
        case "info":
          level = hre.lang.System.LogLevel.Info;
          break;
        case "progress":
          level = hre.lang.System.LogLevel.Progress;
          break;
        case "debug":
          level = hre.lang.System.LogLevel.Debug;
          break;
        case "all":
          level = hre.lang.System.LogLevel.All;
          break;
      }

      if(!debugFilters.get().isEmpty() && level.getOrder() < hre.lang.System.LogLevel.Debug.getOrder()) {
        level = hre.lang.System.LogLevel.Debug;
      }

      for(String filter : debugFilters.get().keySet()) {
        if(filter.contains(":") /* With line number */) {
          hre.lang.System.addDebugFilterByLine(filter);
        } else {
          hre.lang.System.addDebugFilterByClassName(filter);
        }
      }

      hre.lang.System.setOutputStream(System.out, level);
      hre.lang.System.setErrorStream(System.err, level);

      System.setErr(new hre.io.ForbiddenPrintStream(System.err));
      System.setOut(new hre.io.ForbiddenPrintStream(System.out));

      if(version.get()) {
        Output("%s %s", BuildInfo.name(), BuildInfo.version());
        Output("Built by sbt %s, scala %s at %s", BuildInfo.sbtVersion(), BuildInfo.scalaVersion(), Instant.ofEpochMilli(BuildInfo.builtAtMillis()));
        if (!BuildInfo.currentBranch().equals("master")) {
          Output(
                  "On branch %s, commit %s, %s",
                  BuildInfo.currentBranch(),
                  BuildInfo.currentShortCommit(),
                  BuildInfo.gitHasChanges()
          );
        }
        return;
      }

      Hashtable<String,CompilerPass> defined_passes=new Hashtable<String,CompilerPass>();
      Hashtable<String,ValidationPass> defined_checks=new Hashtable<String,ValidationPass>();

      define_passes(silver, separate_checks, defined_passes, defined_checks);

      if (help_passes.get()) {
        Output("The following passes are available:");
        for (Entry<String, CompilerPass> entry:defined_passes.entrySet()){
          Output(" %-12s : %s",entry.getKey(),entry.getValue().getDescripion());
        }
        for (Entry<String, ValidationPass> entry:defined_checks.entrySet()){
          Output(" %-12s : %s",entry.getKey(),entry.getValue().getDescripion());
        }
        throw new HREExitException(0);
      }
      if (CommandLineTesting.enabled()){
        CommandLineTesting.runTests();
        throw new HREExitException(0);
      }
      if (!(boogie.get() || chalice.get() || silver.used() || dafny.get() || pass_list.iterator().hasNext())) {
        Fail("no back-end or passes specified");
      }
      if (silver.used()){
        switch(silver.get()){
        case "silicon_qp":
          Warning("silicon_qp has been merged into silicon, using silicon instead");
          silver.set("silicon");
          break;
        case "silicon":
        case "carbon":
          break;
        default:
          Fail("unknown silver backend: %s",silver.get());
        }
      }
      Progress("parsing inputs...");
      PassReport report=new PassReport(program);
      report.setOutput(program);
      report.add(new ErrorDisplayVisitor());
      int cnt = 0;
      tk.show();
      for(String name : input){
        File f=new File(name);
        if (!no_context.get()){
          FileOrigin.add(name,gui_context.get());
        }
        program.add(Parsers.parseFile(f.getPath()));
        cnt++;
      }
      Progress("Parsed %d file(s) in: %dms",cnt, tk.show());

      if (boogie.get() || sequential_spec.get()) {
        program.setSpecificationFormat(SpecificationFormat.Sequential);
      }
      FeatureScanner features=new FeatureScanner();
      program.accept(features);
      classes=new ArrayList<ClassName>();
      for (ClassName name:program.classNames()){
        classes.add(name);
      }
      Deque<String> passes=null;
      if (pass_list_option.used()){
        passes=new LinkedBlockingDeque<String>();
        for(String s:pass_list){
          passes.add(s);
        }
      } else if (boogie.get()) {
      	passes=new LinkedBlockingDeque<String>();
      	passes.add("java_resolve"); // inspect class path for retreiving signatures of called methods. Will add files necessary to understand the Java code.
        passes.add("standardize"); // a rewriter s.t. only a subset of col will have to be supported
        passes.add("check"); // type check col. Add annotations (the types) to the ast.
        passes.add("rewrite_arrays"); // array generation and various array-related rewrites
        passes.add("check");
        passes.add("flatten"); // expressions that contain method calls (possibly having side-effects) are put into separate statements.
        passes.add("assign");  // '(x = y ==> assign(x,y);). Has not been merged with standardize because flatten needs to be done first.
        passes.add("finalize_args"); // declare new variables to never have to change the arguments (which isn't allowed in silver)
        passes.add("reorder"); // silver requires that local variables are declared at the top of methods (and loop-bodies?) so they're all moved to the top
        passes.add("simplify_calls"); // getting rid of some class names?
        if (infer_modifies.get()) {
          passes.add("standardize");
          passes.add("check");
          passes.add("modifies"); // modifies is mandatory. This is how to automatically add it
        }
        passes.add("standardize");
        passes.add("check");
        passes.add("create-return-parameter"); // all methods in Boogie are void, so use an out parameter instead of 'return..'
        passes.add("standardize");
        passes.add("check");
        passes.add("flatten");
        passes.add("reorder");
        passes.add("standardize");
        passes.add("check");
        passes.add("strip_constructors"); // somewhere in the parser of Java, constructors are added implicitly. They need to be taken out again.
        passes.add("standardize");
        passes.add("check");
      	passes.add("boogie"); // run backend
      } else if (dafny.get()) {
        passes=new LinkedBlockingDeque<String>();
        passes.add("java_resolve");
        passes.add("standardize");
        passes.add("check");
        passes.add("create-return-parameter");
        passes.add("standardize");
        passes.add("check");
        //passes.add("flatten");
        //passes.add("reorder");
        //passes.add("check");
        passes.add("dafny"); // run backend
      } else if (silver.used()||chalice.get()) {
        passes=new LinkedBlockingDeque<String>();

        boolean usesBreakContinue = features.usesSpecial(ASTSpecial.Kind.Break) || features.usesSpecial(ASTSpecial.Kind.Continue);

        if (usesBreakContinue) {
          passes.add("specify-implicit-labels");
        }

        if (features.usesSpecial(ASTSpecial.Kind.Continue)) {
          passes.add("continue-to-break");
        }

        passes.add("java_resolve");
        passes.add("standardize");
        passes.add("java-check");

        if (features.usesSwitch()) {
          passes.add("unfold-switch");
          passes.add("java-check");
        }

        if ((features.usesFinally() || abruptTerminationViaExceptions.get()) && (usesBreakContinue || features.usesReturn())) {
          passes.add("break-return-to-exceptions");
        } else if (usesBreakContinue || features.usesReturn()) {
          passes.add("break-return-to-goto");
        }

        if (features.usesSynchronizedModifier() || features.usesSynchronizedStatement()) {
          passes.add("unfold-synchronized");
        }

        if (silver.used() &&
           ( features.usesSpecial(ASTSpecial.Kind.Lock)
          || features.usesSpecial(ASTSpecial.Kind.Unlock)
          || features.usesSpecial(ASTSpecial.Kind.Fork)
          || features.usesSpecial(ASTSpecial.Kind.Join)
          || features.usesOperator(StandardOperator.PVLidleToken)
          || features.usesOperator(StandardOperator.PVLjoinToken)
          || features.usesSynchronizedStatement()
          || features.usesSynchronizedModifier()
        )){
          passes.add("pvl-encode"); // translate built-in statements into methods and fake method calls.
        }

        passes.add("standardize");
        passes.add("check"); // marking function: stub

        if(features.usesOperator(StandardOperator.AddrOf)) {
          passes.add("lift_declarations");
        }

        passes.add("check");
        passes.add("infer_adt_types");

        passes.add("check");
        passes.add("adt_operator_rewrite");

        passes.add("check");
        passes.add("standardize");

        passes.add("check");
        passes.add("pointers_to_arrays");
        passes.add("check");
        passes.add("desugar_valid_pointer");
        passes.add("check");
        passes.add("array_null_values"); // rewrite null values for array types into None
        passes.add("check");
        if (silver.used()){
          // The new encoding does not apply to Chalice yet.
          // Maybe it never will.
          passes.add("java-encode"); // disambiguate overloaded stuff, copy inherited functions and specifications
          passes.add("standardize");
          passes.add("check");
        }

        if (sat_check.get()) {
          passes.add("sat_check"); // sanity check to avoid uncallable methods (where False is required)
          passes.add("standardize");
          passes.add("check");
        }

        if (features.usesIterationContracts()||features.usesPragma("omp")){
          passes.add("openmp2pvl"); // Converts *all* parallel loops! (And their compositions) Into ordered set of parallel blocks in pvl.
          passes.add("standardize");
          passes.add("check");
        }

        passes.add("propagate-invariants"); // desugar \invariant (see \invariant documentation)
        passes.add("standardize");
        passes.add("check");
        if (features.usesOperator(StandardOperator.Wand)){
          passes.add("magicwand"); // translate magicwand uses (proof oblications) into method calls
          passes.add("standardize");
          passes.add("check");
        }

        passes.add("inline"); // inline predicates that are marked as inline (so 'fold/unfold' needn't be done)
        passes.add("standardize");
        passes.add("check");

        if (features.usesCSL()){
          passes.add("csl-encode"); //
          passes.add("standardize");
          passes.add("check");
        }

        if(features.hasVectorBlocks()||features.usesPragma("omp")){
          passes.add("vector-encode"); // vector operations for supporting Saeed's paper
          passes.add("standardize");
          passes.add("check");
        }

        passes.add("local-variable-check");

        if (silver.used()) {
          if (features.usesIterationContracts()||features.usesParallelBlocks()||features.usesCSL()||features.usesPragma("omp")){
            passes.add("inline-atomic");
            passes.add("check");
            passes.add("parallel_blocks"); // pvl parallel blocks are put in separate methods that can be verified seperately. Method call replaces the contract of this parallel block.
            passes.add("standardize");
          }
          passes.add("check");
          passes.add("simplify_quant"); // reduce nesting of quantifiers
          passes.add("simplify_quant_relations");
          if (features.usesSummation()||features.usesIterationContracts()) {
            passes.add("check");
            passes.add("simplify_sums"); // set of rewrite rules for removing summations
          }
          passes.add("standardize");
          passes.add("check");
        }

        if (features.usesKernels()){// 8 feb 2018: is this now dead code (to be)? (SB)
          passes.add("kernel-split");
          passes.add("check");
          passes.add("simplify_quant");
          passes.add("standardize");
          passes.add("check");
        }

        if (silver.used()) {
          if (  features.usesOperator(StandardOperator.Instance)
            || features.usesInheritance()
            || features.usesOperator(StandardOperator.TypeOf)
          ){
            passes.add("add-type-adt"); // add java's types of the programs as silicon's axiomatic datatypes
            passes.add("standardize");
            passes.add("check");
          }
        }

        if (!silver.used() && features.usesInheritance()){ // 8 feb 2018: SB nominates this block for removal
        	  // reason: chalice's types and inheritance mechanism isn't the same as Java's, so why not translate everything the same way and ignore chalice's mechanism
          passes.add("standardize");
          passes.add("check");
          passes.add("ds_inherit"); // Use the old inheritance encoding for Chalice.
          passes.add("standardize");
          passes.add("check");
        }

        // histories and futures
        // three verification runs:
        // 3) verify program: promises wrt process algebra need to be met
        // 2) verify the process algebra axioms: check whether paralel compositions are 'correct'
        // 1) auxiliarry definitions in the process algebra should match their contracts.
        if (check_defined.get()){
          passes.add("check-defined"); // add checks
          passes.add("standardize");
          passes.add("check");
        } else if (check_axioms.get()){
          passes.add("check-axioms");
          passes.add("standardize");
          passes.add("check");
        } else if (features.usesProcesses() || check_history.get()){
          passes.add("access"); // pre-process for check-history?
          passes.add("standardize");
          passes.add("check");
          passes.add("check-history");
          passes.add("standardize");
          passes.add("check");
        }

        if (explicit_encoding.get()){
          passes.add("explicit_encoding"); // magic wand paper: for passing around predicates witnesses. In chalice predicates do not have arguments, except 'this'. So we're making data-types to pass around witnesses. Not necessary for silicon.
          passes.add("standardize");
          passes.add("check");
        }

        if (silver.used()) {
          passes.add("current_thread"); // add argument 'current thread' to all methods
          passes.add("standardize");
          passes.add("check");
        }

        passes.add("rewrite_arrays"); // array generation and various array-related rewrites
        passes.add("check");
        passes.add("generate_adt_functions");
        passes.add("check");
        passes.add("flatten");
        passes.add("check");
        passes.add("intro-exc-var");
        passes.add("check");
        passes.add("encode-try-throw-signals");
        passes.add("check");
        passes.add("assign");
        passes.add("reorder");
        passes.add("standardize");

        passes.add("check");
        passes.add("simplify_quant");
        passes.add("standardize");
        passes.add("check");

        if (silver.used()) {
          // Part of this is now done in java-encode
          // The remainder is shifted to silver-class-reduction
          //passes.add("class-conversion");
          //passes.add("standardize");
          //passes.add("check");

          if(trigger_generation.get() > 0) {
            passes.add("simple_triggers=" + trigger_generation.get());
            passes.add("check");
          }
          passes.add("silver-class-reduction"); // remove the class (since all names are now unique), only one class remains
          passes.add("standardize");
          passes.add("check");
        } else {
          passes.add("globalize"); // create a separate class to contain all statics (class probably called 'Global', needs to be given as argument to the other methods)
          passes.add("standardize");
          passes.add("check");
          passes.add("rm_cons"); // replace constructors by init-methods
          passes.add("standardize");
          passes.add("check");
        }

        passes.add("create-return-parameter");
        passes.add("check");

        passes.add("standardize");
        passes.add("check");

        if (silver.used()) {
          passes.add("ghost-lift"); // change ghost code into real code so it can is used in the check
          passes.add("standardize");
          passes.add("check");
        }

        passes.add("flatten");
        passes.add("reorder"); // leaves declarations at the top of blocks (within loops and branches)
        passes.add("flatten_before_after"); // method calls can have 'before/after' blocks of ghost-code attached. Put it around all method calls.
        if (silver.used()) {
          passes.add("silver-reorder"); // no declarations in branches (only in loops)
          // passes.add("silver-identity"); // identity functions are used to not optimize expressions. Add it to silver.
        }
        passes.add("standardize");
        passes.add("check");


        if (!silver.used())  {
          passes.add("finalize_args");
          passes.add("reorder");
          passes.add("standardize");
          passes.add("check");
        }

        if (silver.used()) {
          passes.add("scale-always"); // syntax: in silicon [p]predicate() is mandatory, so change pred() to [1]pred()
          passes.add("check"); // the rewrite system needs a type check
          passes.add("silver-optimize"); // rewrite to things that silver likes better
          passes.add("check"); // the rewrite system needs a type check
          passes.add("quant-optimize"); // some illegal-quantifier constructions need to be written differently (plus optimize)
          passes.add("standardize-functions"); // pure methods do not need to be 'methods', try turning them into functions so silver and chalice can reason more intelligently about them. Pure methods can be used in specifications through this.
          passes.add("standardize");
          passes.add("check");
          passes.add("inline-pattern-to-trigger");
          passes.add("gen-triggers");
          passes.add("check");

          passes.add("silver");
        } else { //CHALICE
          passes.add("check"); // rewrite system needs a type check
          passes.add("chalice-optimize");
          passes.add("standardize-functions");
          passes.add("standardize");
          passes.add("check");

          passes.add("chalice-preprocess");
          passes.add("standardize");
          passes.add("check");
          passes.add("chalice");
        }
      } else {
      	Abort("no back-end or passes specified");
      }
      if(learn.get()) {
        passes.addFirst("count=" + silver.get() + "_before_rewrite");
        passes.add("learn=" + wallStart);
      }
      {
        int fatal_errs=0;
        @SuppressWarnings("unused")
        ArrayList<PassReport> reports=new ArrayList<PassReport>();
        int passCount = passes.size();

        while(!passes.isEmpty() && fatal_errs==0){
          String pass=passes.removeFirst();
          String[] pass_args=pass.split("=");
          pass=pass_args[0];
          if (pass_args.length==1){
            pass_args=new String[0];
          } else {
            // Arg syntax: pass=arg1\arg2\arg3. Make sure to put plenty of backslashes there!
            pass_args=pass_args[1].split("\\\\+");
          }
          CompilerPass task=defined_passes.get(pass);
          if(debugBefore.has(pass)) {
            program.dump();
          }
          if (show_before.contains(pass)){
            String name=show_file.get();
            if (name!=null){
              String file=String.format(name, pass);
              PrintWriter out=new PrintWriter(new FileOutputStream(file));
              vct.col.ast.util.Configuration.getDiagSyntax().print(out,program);
              out.close();
            } else {
              PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
              vct.col.ast.util.Configuration.getDiagSyntax().print(out, program);
              out.close();
            }
          }
          if (task!=null){
            tk.show();
            report=task.apply_pass(report,pass_args);
            fatal_errs=report.getFatal();
            program=report.getOutput();
            Progress("[%02d%%] %s took %d ms",100*(passCount-passes.size())/passCount, pass, tk.show());
          } else {
            ValidationPass check=defined_checks.get(pass);
            if (check!=null){
              DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");
              Progress("Applying %s at %s ...", pass, ZonedDateTime.now().format(formatter));
              tk.show();
              report=check.apply_pass(report,pass_args);
              fatal_errs=report.getFatal();
              Progress(" ... pass took %d ms", tk.show());
            } else {
              Fail("unknown pass %s",pass);
            }
          }
          if(debugAfter.has(pass)) {
            program.dump();
          }
          if (show_after.contains(pass)){
            String name=show_file.get();
            if (name!=null){
              String file=String.format(name, pass);
              PrintWriter out=new PrintWriter(new FileOutputStream(file));
              vct.col.ast.util.Configuration.getDiagSyntax().print(out,program);
              out.close();
            } else {
              PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
              vct.col.ast.util.Configuration.getDiagSyntax().print(out,program);
              out.close();
            }
          }
          if (stop_after.contains(pass)){
            Fail("exit after pass %s",pass);
          }
        }
        Verdict("The final verdict is %s",fatal_errs==0?"Pass":"Fail");
        //report.listFatals();
      }
    } catch (HREExitException e) {
      exit=e.exit;
      Verdict("The final verdict is Error");
    } catch (Throwable e) {
      DebugException(e);
      Verdict("An unexpected error occured in VerCors! " +
              "Please report an issue at https://github.com/utwente-fmt/vercors/issues/new. " +
              "You can see the full exception by adding '--debug vct.main.Main' to the flags.");
      Verdict("The final verdict is Error");
      throw e;
    } finally {
      Progress("entire run took %d ms",System.currentTimeMillis()-wallStart);
      System.exit(exit);
    }
  }

  private static void define_passes(
      final StringSetting silver,
      final BooleanSetting separate_checks,
      Hashtable<String, CompilerPass> defined_passes,
      Hashtable<String, ValidationPass> defined_checks) {
    defined_passes.put("java",new CompilerPass("print AST in java syntax"){
        public ProgramUnit apply(ProgramUnit arg,String ... args){
          PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
          JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out,arg);
          out.close();
          return arg;
        }
      });
    defined_passes.put("c",new CompilerPass("print AST in C syntax"){
        public ProgramUnit apply(ProgramUnit arg,String ... args){
          PrintWriter out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
          vct.col.ast.print.CPrinter.dump(out,arg);
          out.close();
          return arg;
        }
      });
    defined_passes.put("add-type-adt",new CompilerPass("Add an ADT that describes the types and use it to implement instanceof"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new AddTypeADT(arg).rewriteAll();
      }
    });
    compiler_pass(defined_passes,"access","convert access expressions for histories/futures",AccessIntroduce.class);
    defined_passes.put("assign",new CompilerPass("change inline assignments to statements"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new AssignmentRewriter(arg).rewriteAll();
      }
    });
    defined_checks.put("silver",new ValidationPass("verify input with Silver"){
      @Override
      public PassReport apply_pass(PassReport arg,String ... args){
        return vct.silver.SilverBackend.TestSilicon(arg,silver.get());
      }
    });
    defined_passes.put("check",new CompilerPass("run a basic type check"){
      @Override
      public ProgramUnit apply(PassReport report, ProgramUnit arg,String ... args){
        new SimpleTypeCheck(report, arg).check();
        return arg;
      }
    });
    defined_passes.put("array_null_values", new CompilerPass("rewrite null values for arrays to None") {
      public ProgramUnit apply(ProgramUnit arg, String... args) {
          return new ArrayNullValues(arg).rewriteAll();
      }
    });
    defined_passes.put("pointers_to_arrays", new CompilerPass("rewrite pointers to arrays") {
      public ProgramUnit apply(ProgramUnit arg, String... args) {
        return new PointersToArrays(arg).rewriteAll();
      }
    });
    defined_passes.put("desugar_valid_pointer", new CompilerPass("rewrite \\array, \\matrix, \\pointer and \\pointer_index") {
      @Override
      protected ProgramUnit apply(ProgramUnit arg, String... args) {
        return new DesugarValidPointer(arg).rewriteAll();
      }
    });
    defined_passes.put("lift_declarations", new CompilerPass("lift declarations to cell of the declared types, to treat locals as heap locations.") {
      @Override
      public ProgramUnit apply(ProgramUnit arg, String... args) {
        return new LiftDeclarations(arg).rewriteAll();
      }
    });
    defined_passes.put("java-check",new CompilerPass("run a Java aware type check"){
      @Override
      public ProgramUnit apply(PassReport report, ProgramUnit arg,String ... args){
        new JavaTypeCheck(report, arg).check();
        return arg;
      }
    });
    defined_passes.put("check-defined",new CompilerPass("rewrite process algebra class to check if defined process match their contracts"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        ProgramUnit tmp=new CheckProcessAlgebra(arg).rewriteAll();
        return new RandomizedIf(tmp).rewriteAll();
      }
    });
    defined_passes.put("check-axioms",new CompilerPass("rewrite process algebra class to check if history axioms are correct"){
      @Override
      public PassReport apply_pass(PassReport arg,String ... args){
        ProgramUnit input=arg.getOutput();
        PassReport res=new PassReport(input);
        ErrorMapping map=new ErrorMapping(arg);
        res.add(map);
        res.setOutput(new CheckHistoryAlgebra(input,Mode.AxiomVerification,map).rewriteAll());
        return res;
      }
    });
    defined_passes.put("check-history",new CompilerPass("rewrite process algebra class to check if history accounting is correct"){
      @Override
      public PassReport apply_pass(PassReport arg,String ... args){
        ProgramUnit input=arg.getOutput();
        PassReport res=new PassReport(input);
        ErrorMapping map=new ErrorMapping(arg);
        res.add(map);
        res.setOutput(new CheckHistoryAlgebra(input,Mode.ProgramVerification,map).rewriteAll());
        return res;
      }
    });
    defined_passes.put("csl-encode",new CompilerPass("Encode CSL atomic regions with methods"){
      @Override
      public PassReport apply_pass(PassReport arg,String ... args){
        ProgramUnit input=arg.getOutput();
        PassReport res=new PassReport(input);
        ErrorMapping map=new ErrorMapping(arg);
        res.add(map);
        res.setOutput(new CSLencoder(input,map).rewriteAll());
        return res;
      }
    });
    defined_passes.put("class-conversion",new CompilerPass("Convert classes into records and procedures"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ClassConversion(arg).rewriteAll();
      }
    });
    defined_passes.put("codegen",new CompilerPass("Generate code"){
      public ProgramUnit apply(PassReport report,ProgramUnit arg,String ... args){
        File dir=new File(args[0]);
        if (dir.exists()){
          if (!dir.isDirectory()){
            report.fatal("%s is not a directory",dir);
            return arg;
          }
        } else {
          if (!dir.mkdirs()){
            report.fatal("could not create %s",dir);
            return arg;
          }
        }
        Syntax syntax=JavaSyntax.getJava(JavaDialect.JavaVerCors);
        for(ASTNode node:arg){
          if (node instanceof ASTClass){
            PrintWriter out;
            try {
              out = new PrintWriter(new FileOutputStream(new File(dir,((ASTClass)node).name() + ".java")));
            } catch (FileNotFoundException e) {
              report.add(new ExceptionMessage(e));
              return arg;
            }
            out.println("import col.lang.*;");
            syntax.print(out, node);
            out.close();
           } else if(node instanceof ASTSpecial){
            ASTSpecial S = (ASTSpecial)node;
            switch(S.kind){
            case Comment:
              // TODO keep comments.
              break;
            default:
              report.fatal("cannot deal with special %s yet",S.kind);
              return arg;
            }
          } else {
            report.fatal("cannot deal with %s yet",node.getClass());
            return arg;
          }
        }
        return arg;
      }
    });
    defined_passes.put("current_thread",new CompilerPass("Encode references to current thread."){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new CurrentThreadRewriter(arg).rewriteAll();
      }
    });
    defined_passes.put("java-encode",new CompilerPass("Encode Java overloading and inheritance"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        arg=new JavaEncoder(arg).rewriteAll();

        return arg;
      }
    });
    defined_passes.put("erase",new CompilerPass("Erase generic types"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        arg=new GenericPass1(arg).rewriteAll();
        return arg;
      }
    });
    defined_passes.put("explicit_encoding",new CompilerPass("encode required and ensured permission as ghost arguments"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ExplicitPermissionEncoding(arg).rewriteAll();
      }
    });
    defined_passes.put("finalize_args",new CompilerPass("???"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new FinalizeArguments(arg).rewriteAll();
      }
    });
    defined_passes.put("flatten",new CompilerPass("remove nesting of expression"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new Flatten(arg).rewriteAll();
      }
    });
    defined_passes.put("ghost-lift",new CompilerPass("Lift ghost code to real code"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new GhostLifter(arg).rewriteAll();
      }
    });
    defined_passes.put("globalize",new CompilerPass("split classes into static and dynamic parts"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new GlobalizeStaticsParameter(arg).rewriteAll();
      }
    });
    defined_passes.put("ds_inherit",new CompilerPass("rewrite contracts to reflect inheritance, predicate chaining"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new DynamicStaticInheritance(arg).rewriteOrdered();
      }
    });
    defined_passes.put("flatten_before_after",new CompilerPass("move before/after instructions"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new FlattenBeforeAfter(arg).rewriteAll();
      }
    });
    defined_passes.put("flatten_variable_declarations", new CompilerPass("put the base type in declarations") {
      public ProgramUnit apply(ProgramUnit arg, String... args) { return new FlattenVariableDeclarations(arg).rewriteAll(); }
    });
    defined_passes.put("inline",new CompilerPass("Inline all methods marked as inline"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new InlinePredicatesRewriter(arg).rewriteAll();
      }
    });
    defined_passes.put("kernel-split",new CompilerPass("Split kernels into main, thread and barrier."){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new KernelRewriter(arg).rewriteAll();
      }
    });
    defined_passes.put("pvl-encode",new CompilerPass("Encode PVL builtins for verification."){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new PVLEncoder(arg).rewriteAll();
      }
    });
    branching_pass(
        defined_passes,
        "magicwand",
        "Encode magic wand proofs with abstract predicates",
        WandEncoder.class
        );
    defined_passes.put("modifies",new CompilerPass("Derive modifies clauses for all contracts"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        new DeriveModifies().annotate(arg);
        return arg;
      }
    });
    defined_passes.put("openmp2pvl",new CompilerPass("Compile OpenMP pragmas to PVL"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new OpenMPToPVL(arg).rewriteAll();
      }
    });
    defined_passes.put("inline-atomic",new CompilerPass("Inlines atomic blocks into inhales/exhales"){
      @Override
      public PassReport apply_pass(PassReport arg,String ... args){
        ProgramUnit input = arg.getOutput();
        PassReport res = new PassReport(input);
        ErrorMapping map = new ErrorMapping(arg);
        res.add(map);
        res.setOutput(new InlineAtomic(input,map).rewriteAll());
        return res;
      }
    });
    defined_passes.put("parallel_blocks",new CompilerPass("Encoded the proof obligations for parallel blocks"){
        @Override
        public PassReport apply_pass(PassReport arg,String ... args){
          ProgramUnit input=arg.getOutput();
          PassReport res=new PassReport(input);
          ErrorMapping map=new ErrorMapping(arg);
          res.add(map);
          res.setOutput(new ParallelBlockEncoder(input,map).rewriteAll());
          return res;
        }

    });
    defined_passes.put("local-variable-check", new CompilerPass("Checks if local variables are not written to when shared between parallel blocks or used in invariants") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        LocalVariableChecker.check(arg);
        return arg;
      }
    });
    defined_passes.put("pvl-compile",new CompilerPass("Compile PVL classes to Java classes"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new PVLCompiler(arg).rewriteAll();
      }
    });
    defined_passes.put("reorder",new CompilerPass("reorder statements (e.g. all declarations at the start of a bock"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ReorderAssignments(arg).rewriteAll();
      }
    });
   defined_passes.put("standardize-functions",new CompilerPass("translate pure methods to function syntax."){
     public ProgramUnit apply(ProgramUnit arg,String ... args){
       return new PureMethodsAsFunctions(arg).rewriteAll();
     }
   });
   defined_passes.put("java_resolve",new CompilerPass("Resolve the library dependencies of a java program"){
     public ProgramUnit apply(ProgramUnit arg,String ... args){
       return new JavaResolver(arg).rewriteAll();
     }
   });
   defined_passes.put("propagate-invariants",new CompilerPass("propagate invariants"){
     public ProgramUnit apply(ProgramUnit arg,String ... args){
       return new PropagateInvariants(arg).rewriteAll();
     }
   });
   defined_passes.put("quant-optimize",new CompilerPass("insert satisfyability checks for all methods"){
     public ProgramUnit apply(ProgramUnit arg,String ... args){
       return new OptimizeQuantifiers(arg).rewriteAll();
     }
   });
   defined_passes.put("rewrite",new CompilerPass("Apply a term rewrite system"){
     public ProgramUnit apply(ProgramUnit arg,String ... args){
       RewriteSystem trs=RewriteSystems.getRewriteSystem(args[0]);
       return trs.normalize(arg);
     }
   });
   defined_passes.put("rewrite_arrays",new CompilerPass("rewrite arrays to sequences of cells"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new RewriteArrayRef(arg).rewriteAll();
      }
    });
    defined_passes.put("generate_adt_functions",new CompilerPass("rewrite  standard operators on sequences to function definitions/calls"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new GenerateADTFunctions(arg).rewriteAll();
      }
    });
    defined_passes.put("infer_adt_types",new CompilerPass("Transform typeless collection constructors by inferring their types."){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new InferADTTypes(arg).rewriteAll();
      }
    });
    defined_passes.put("adt_operator_rewrite",new CompilerPass("rewrite PVL-specific ADT operators"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ADTOperatorRewriter(arg).rewriteAll();
      }
    });
    defined_passes.put("rm_cons",new CompilerPass("???"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ConstructorRewriter(arg).rewriteAll();
      }
    });
    defined_passes.put("sat_check",new CompilerPass("insert satisfyability checks for all methods"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new SatCheckRewriter(arg).rewriteAll();
      }
    });
    defined_passes.put("setget",new CompilerPass("insert set and get operators"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new SetGetIntroduce(arg).rewriteAll();
      }
    });
    defined_passes.put("silver-class-reduction",new CompilerPass("reduce classes to single Ref class"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new SilverClassReduction(arg).rewriteAll();
      }
    });
    defined_passes.put("silver-reorder",new CompilerPass("move declarations from inside if-then-else blocks to top"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new SilverReorder(arg).rewriteAll();
      }
    });
    defined_passes.put("scale-always",new CompilerPass("scale every predicate invokation"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ScaleAlways(arg).rewriteAll();
      }
    });
    defined_passes.put("silver-identity",new CompilerPass("Implement identity operator for Silver"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new SilverImplementIdentity(arg).rewriteAll();
      }
    });
    defined_passes.put("silver-optimize",new CompilerPass("Optimize expressions for Silver"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        RewriteSystem trs=RewriteSystems.getRewriteSystem("silver_optimize");
        return trs.normalize(arg);
      }
    });
    defined_passes.put("chalice-optimize",new CompilerPass("Optimize expressions for Chalice"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        RewriteSystem trs=RewriteSystems.getRewriteSystem("chalice_optimize");
        return trs.normalize(arg);
      }
    });
    defined_passes.put("simplify_calls",new CompilerPass("???"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new SimplifyCalls(arg).rewriteAll();
      }
    });
    defined_passes.put("simplify_expr",new CompilerPass("Simplify expressions"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        RewriteSystem trs=RewriteSystems.getRewriteSystem("simplify_expr");
        return trs.normalize(arg);
      }
    });
    defined_passes.put("simplify_quant",new CompilerPass("Simplify quantifications"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        RewriteSystem trs=RewriteSystems.getRewriteSystem("simplify_quant_pass1");
        ProgramUnit res=trs.normalize(arg);
        res=RewriteSystems.getRewriteSystem("simplify_quant_pass2").normalize(res);
        return res;
      }
    });
    defined_passes.put("simplify_sums",new CompilerPass("replace summations with provable functions"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        RewriteSystem trs=RewriteSystems.getRewriteSystem("summation");
        return trs.normalize(arg);
      }
    });
    defined_passes.put("simplify_quant_relations", new CompilerPass("simplify quantified relational expressions") {
      public ProgramUnit apply(ProgramUnit arg, String... args) {
        return new SimplifyQuantifiedRelations(arg).rewriteAll();
      }
    });
    defined_passes.put("standardize",new CompilerPass("Standardize representation"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new Standardize(arg).rewriteAll();
      }
    });
    defined_passes.put("strip_constructors",new CompilerPass("Strip constructors from classes"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new StripConstructors(arg).rewriteAll();
      }
    });
    branching_pass(defined_passes,"create-return-parameter","Replace return value by out parameter.",CreateReturnParameter.class);
    compiler_pass(defined_passes,"vector-encode","Encode vector blocks using the vector library",VectorEncode.class);
    defined_passes.put("chalice-preprocess",new CompilerPass("Pre processing for chalice"){
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ChalicePreProcess(arg).rewriteAll();
      }
    });
    defined_passes.put("simple_triggers", new CompilerPass("Add triggers to quantifiers if possible") {
      public ProgramUnit apply(ProgramUnit arg, String... args) {
        ProgramUnit res = arg;
        int val = Integer.valueOf(args[0]);
        // First gather quantified variables for quantifiers without triggers.
        res = new OptimizeQuantifiers(res).rewriteAll();
        // For quantifiers without triggers, and complex subscripts not containing quantified variables, add quantifier variable equal to the complex subscript.
        if ((val & 2) > 0) {
          res = new RewriteComplexUnitSubscripts(res).rewriteAll();
        }
        // Try to add triggers for the now possibly simplified quantifiers.
        if ((val & 1) > 0) {
          res = new AddSimpleTriggers(res).rewriteAll();
        }
        return res;
      }
    });
    defined_passes.put("count",new CompilerPass("Count nodes."){
        public ProgramUnit apply(ProgramUnit arg,String ... args){
          NonLinCountVisitor cv = new NonLinCountVisitor(arg);
          cv.count();
          if(args.length == 1) {
            counters.put(args[0], cv);
          } else {
            Abort("Learn is used without an oracle");
          }
          return arg;
        }
      });
    defined_passes.put("learn",new CompilerPass("Learn unit times from counted AST nodes."){
        public ProgramUnit apply(ProgramUnit arg,String ... args){
          if(args.length == 1) {
            long start_time = Long.valueOf(args[0]);
            long time = System.currentTimeMillis() - start_time;
            for(Map.Entry<String, SpecialCountVisitor> entry: counters.entrySet()) {
              Oracle.tell(entry.getKey(), entry.getValue(), time);
            }
          } else {
            Abort("Learn is used without a starting time.");
          }
          return arg;
        }
      });
    defined_passes.put("gen-triggers", new CompilerPass("") {
      public ProgramUnit apply(ProgramUnit arg, String... args) {
        return new Triggers(arg).rewriteAll();
      }
    });
    defined_passes.put("inline-pattern-to-trigger", new CompilerPass("Explicit inline patterns to normal trigger syntax") {
      public ProgramUnit apply(ProgramUnit arg, String... args) {
        return new InlinePatternToTrigger(arg).rewriteAll();
      }
    });
    defined_passes.put("specify-implicit-labels", new CompilerPass("Insert explicit labels for break statements in while loops.") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new SpecifyImplicitLabels(arg).rewriteAll();
      }
    });
    defined_passes.put("break-return-to-goto", new CompilerPass("Rewrite break, return into jumps") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new BreakReturnToGoto(arg).rewriteAll();
      }
    });
    defined_passes.put("break-return-to-exceptions", new CompilerPass("Rewrite break, continue into exceptions") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new BreakReturnToExceptions(arg).rewriteAll();
      }
    });
    defined_passes.put("unfold-switch", new CompilerPass("Unfold switch to chain of if-statements that jump to sections.") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new UnfoldSwitch(arg).rewriteAll();
      }
    });
    defined_passes.put("continue-to-break", new CompilerPass("Convert continues into breaks") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new ContinueToBreak(arg).rewriteAll();
      }
    });
    defined_passes.put("unfold-synchronized", new CompilerPass("Convert synchronized to try-finally") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args){
        return new UnfoldSynchronized(arg).rewriteAll();
      }
    });
    defined_passes.put("intro-exc-var", new CompilerPass("Introduces the auxiliary sys__exc variable for use by excetional control flow") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args) {
        return new IntroExcVar(arg).rewriteAll();
      }
    });
    defined_passes.put("encode-try-throw-signals", new CompilerPass("Encodes exceptional control flow into gotos and exceptional contracts into regular contracts") {
      @Override
      public ProgramUnit apply(ProgramUnit arg,String ... args) {
        return new EncodeTryThrowSignals(arg).rewriteAll();
      }
    });
  }


  private static void branching_pass(Hashtable<String, CompilerPass> defined_passes,
  String key, String description, final Class<? extends AbstractRewriter> class1) {
try {
  defined_passes.put(key,new CompilerPass(description){

    Constructor<? extends AbstractRewriter> cons=class1.getConstructor(ProgramUnit.class,ErrorMapping.class);

    @Override
    public PassReport apply_pass(PassReport inrep, String... args) {
      ProgramUnit arg=inrep.getOutput();
      PassReport res=new PassReport(arg);
      ErrorMapping map=new ErrorMapping(inrep);
      res.add(map);
      AbstractRewriter rw;
      try {
        rw = (AbstractRewriter) cons.newInstance(arg,map);
      } catch (Exception e) {
        throw new HREError("unexpected exception %s",e);
      }
      res.setOutput(rw.rewriteAll());
      return res;
    }

  });
} catch (NoSuchMethodException e) {
  Abort("bad rewriter pass %s",key);
}
}

  private static void compiler_pass(Hashtable<String, CompilerPass> defined_passes,
      String key, String description, final Class<? extends AbstractRewriter> class1) {
    try {
      defined_passes.put(key,new CompilerPass(description){

        Constructor<? extends AbstractRewriter> cons=class1.getConstructor(ProgramUnit.class);

        @Override
        public ProgramUnit apply(ProgramUnit arg, String... args) {
          AbstractRewriter rw;
          try {
            rw = (AbstractRewriter) cons.newInstance(arg);
          } catch (Exception e) {
            throw new HREError("unexpected exception %s",e);
          }
          return rw.rewriteAll();
        }

      });
    } catch (NoSuchMethodException e) {
      Abort("bad rewriter pass %s",key);
    }
  }
}
