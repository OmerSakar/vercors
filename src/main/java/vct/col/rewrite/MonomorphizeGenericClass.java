package vct.col.rewrite;

import scala.collection.JavaConverters;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.util.GenericsScanner;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class MonomorphizeGenericClass extends AbstractRewriter {

    private DeclarationStatement field;

    public MonomorphizeGenericClass(ProgramUnit source) {
        super(source);
    }

    private Map<ClassType, List<List<Type>>> genericsTypes = new HashMap<>();

    @Override
    public ProgramUnit rewriteAll() {
        ProgramUnit res = super.rewriteAll();

        for (ClassType classType: genericsTypes.keySet()) {
            GenerateGenericClassInstance genericClassInstance = new GenerateGenericClassInstance();
            for (List<Type> mylist: genericsTypes.get(classType)) {
                ASTClass classToGenerate = source().find(classType);
                res.add(genericClassInstance.rewrite(classToGenerate, mylist));
            }
        }
        return res;
    }

    @Override
    public void visit(MethodInvokation e) {
//        super.visit(e);
        StringBuilder generatedName = new StringBuilder(String.join("", JavaConverters.seqAsJavaList(e.dispatch.names())));
        if (e.dispatch.params().length() > 0) {
            for (ASTNode type: JavaConverters.seqAsJavaList(e.dispatch.params())) {
                generatedName.append("_").append(type.toString());
            }
        }


        result = create.new_object(create.class_type(generatedName.toString()), rewrite(e.getArgs()));
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
        GenericsScanner scanner = new GenericsScanner();
        c.apply(scanner);
        Map<ClassType, List<List<Type>>> mapping = scanner.getMappings();

        for (ClassType classType: mapping.keySet()) {
            if (genericsTypes.containsKey(classType)) {
                genericsTypes.get(classType).addAll(mapping.get(classType));
            } else {
                genericsTypes.put(classType, mapping.get(classType));
            }
        }


//        val fieldCollector = new FieldAccessCollector
//        setComprehension.main.apply(fieldCollector)
//
//        val fieldAccessses = fieldCollector.getFieldAccesses


//        if (c.kind == ASTClass.ClassKind.Abstract && c.parameters != null && c.parameters.length > 0) {
//            for (DeclarationStatement clazzDecl : c.parameters) {
//                if (!genericsTypes.containsKey(clazzDecl.name())) {
//                    genericsTypes.put(
//                            clazzDecl.name(), create.ast_class(abstractClassPrefix + clazzDecl.name(), ASTClass.ClassKind.Abstract, null, null, null)
//                    );
//                }
//            }
//        }

//        DeclarationStatement diz = new DeclarationStatement("diz", new ClassType("classname"));
//
//        ASTNode body = create.constant(true);
//        for (DeclarationStatement field: c.dynamicFields()) {
//            body = and(
//                    body,
//                    create.expression(StandardOperator.Value,
//                                create.dereference(
//                                        create.local_name(diz.name()),
//                                        field.name()
//                                )
//                    )
//            );
//            if (field.type() instanceof ClassType) {
//                body = and(
//                        body,
//                        create.invokation(null, null, "thenameofthepredicatethatyougenerate", create.dereference(
//                                create.local_name(diz.name()),
//                                field.name()
//                        ))
//
//                );
//            }
//        }
//        create.expression(
//                StandardOperator.Value,
//                create.dereference(create.local_name(clazz.name),
//                        field.field)

//        create.predicate("thenameoftheclass+theinstancesoftypeparameters", body, new DeclarationStatement(, ));
    }

}
