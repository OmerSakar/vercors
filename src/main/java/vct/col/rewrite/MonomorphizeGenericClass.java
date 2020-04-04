package vct.col.rewrite;

import scala.collection.JavaConverters;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.ast.util.RecursiveVisitor;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class MonomorphizeGenericClass extends AbstractRewriter {

    private DeclarationStatement field;

    public MonomorphizeGenericClass(ProgramUnit source) {
        super(source);
    }

    private Map<ClassType, Set<List<Type>>> genericsTypes = new HashMap<>();

    @Override
    public ProgramUnit rewriteAll() {
        ProgramUnit res = super.rewriteAll();

        for (ClassType classType: genericsTypes.keySet()) {
            GenerateGenericClassInstance genericClassInstance = new GenerateGenericClassInstance();
            for (List<Type> mylist: genericsTypes.get(classType)) {
                ASTClass classToGenerate = source().find(classType);
                ASTClass generatedClass = genericClassInstance.rewrite(classToGenerate, mylist);
                if (!StreamSupport.stream(generatedClass.dynamicMethods().spliterator(), false) .anyMatch(m -> m.kind == Method.Kind.Constructor)) {
                    create.addZeroConstructor(generatedClass);
                }
                res.add(generatedClass);
            }
        }
        return res;
    }

    @Override
    public void visit(MethodInvokation e) {
        if (e.dispatch != null && e.dispatch.params()!= null && !e.dispatch.params().isEmpty() && Method.JavaConstructor.equals(e.method)) {
            StringBuilder generatedName = new StringBuilder(String.join("", JavaConverters.seqAsJavaList(e.dispatch.names())));
            if (e.dispatch.params().length() > 0) {
                for (ASTNode type : JavaConverters.seqAsJavaList(e.dispatch.params())) {
                    generatedName.append("_").append(type.toString());
                }
            }
            result = create.new_object(create.class_type(generatedName.toString()), rewrite(e.getArgs()));
        } else {
            super.visit(e);
        }
    }


    @Override
    public void visit(ClassType t) {
        if (t.params() != null && !t.params().isEmpty()) {
            StringBuilder generatedName = new StringBuilder(t.getName());

            for (ASTNode type : JavaConverters.seqAsJavaList(t.params())) {
                generatedName.append("_").append(type.toString());
            }
            result = create.class_type(generatedName.toString());
        } else {
            super.visit(t);
        }
    }

    @Override
    public void visit(ASTClass c) {
        super.visit(c);
        GenericsScanner scanner = new GenericsScanner(this.source());
        c.apply(scanner);
        Map<ClassType, Set<List<Type>>> mapping = scanner.getMappings();

        for (ClassType classType: mapping.keySet()) {
            if (genericsTypes.containsKey(classType)) {
                genericsTypes.get(classType).addAll(mapping.get(classType));
            } else {
                genericsTypes.put(classType, mapping.get(classType));
            }
        }
    }

    private class GenericsScanner extends RecursiveVisitor<Object> {

        private Map<ClassType, Set<List<Type>>> mappings = new HashMap<>();
        private List<MethodInvokation> staticMethodInvokations = new ArrayList<>();

        public GenericsScanner(ProgramUnit source) {
            super(source, null);
        }

        @Override
        public void visit(ASTClass c) {
            if (c.kind == ASTClass.ClassKind.Abstract && c.parameters != null && c.parameters.length > 0){
                return;
            }
            super.visit(c);
        }

        @Override
        public void visit(ClassType t) {
            if (t.params() != null && !t.params().isEmpty()) {
                List<Type> typeParams = JavaConverters.seqAsJavaList(t.params()).stream().map(t1 -> (Type) t1).collect(Collectors.toList());
                Set<List<Type>> mappings = this.mappings.getOrDefault(t, new HashSet<>());
                mappings.add(typeParams);
                this.mappings.put(t, mappings);
            } else {
                super.visit(t);
            }
        }

        public Map<ClassType, Set<List<Type>>> getMappings() {
            return mappings;
        }

        public List<MethodInvokation> getStaticMethodInvokations() {
            return staticMethodInvokations;
        }
    }
}
