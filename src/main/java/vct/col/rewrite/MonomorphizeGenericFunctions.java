package vct.col.rewrite;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.Type;
import vct.col.ast.type.TypeVariable;
import vct.col.ast.util.AbstractRewriter;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class MonomorphizeGenericFunctions extends AbstractRewriter {
    public List<MethodInvokation> invokationsOfAbstractFunctions = new ArrayList<>();

    public MonomorphizeGenericFunctions(ProgramUnit source) {
        super(source);
    }

    @Override
    public ProgramUnit rewriteAll() {
        ProgramUnit res = super.rewriteAll();

        Map<Method, Map<ClassType, Type>> allTheFunctionsToGenerate = new HashMap<>();

        for (MethodInvokation mi: invokationsOfAbstractFunctions) {
            List<AbstractMap.SimpleEntry<Type, Type>> tmp = IntStream.range(0, mi.getArgs().length)
                    .mapToObj(i -> new AbstractMap.SimpleEntry<>(mi.getDefinition().getArgType(i), mi.getArg(i).getType()))
                    .collect(Collectors.toList());

            Map<ClassType, Type> mapping = new HashMap<>();
            List<TypeVariable> typeParams = Arrays.stream(mi.getDefinition().typeParameters).map(d -> (TypeVariable) d.getType()).collect(Collectors.toList());
            for (AbstractMap.SimpleEntry<Type, Type> tt: tmp) {
                Map<ClassType, Type> mappingForArg = MonomorphizeGenericFunctions.matchUpToTypeParameters(tt.getKey(), tt.getValue(), typeParams);
                if (mappingForArg == null) {
                    Fail("Type parameter bound to two different types.");
                }
                mapping.putAll(mappingForArg);
            }
            if(!mapping.entrySet().stream().allMatch(e -> e.getKey().equals(e.getValue()))) {
                allTheFunctionsToGenerate.put(mi.getDefinition(), mapping);
            }
        }

        allTheFunctionsToGenerate.forEach((k,v) -> res.find(((ASTClass) k.getParent()).getName()).add(new GenerateGenericFunctionInstance().rewrite(k, v)));

        return res;
    }

    @Override
    public void visit(ASTClass c) {
        if (c.parameters.length == 0) {
            super.visit(c);
        } else {
            result = c;
        }
    }

    @Override
    public void visit(Method m) {
        if (m.typeParameters != null && m.typeParameters.length == 0) {
            super.visit(m);
        } else {
            result = m;
            result.clearParent();
        }
    }


    @Override
    public void visit(MethodInvokation e) {
        if (e.getDefinition().typeParameters.length != 0) {
            invokationsOfAbstractFunctions.add(e);

            StringBuilder generatedName = new StringBuilder(e.method);
            if (e.getArgs() != null && e.getArgs().length > 0) {
                for (ASTNode arg : e.getArgs()) {
                    generatedName.append("_").append(arg.getType().toString());
                }
            }
            generatedName.append("_").append(e.getType().toString());

            result = create.invokation(rewrite(e.object), rewrite(e.dispatch), generatedName.toString(), e.getArgs());
        } else {
            super.visit(e);
        }
    }

    /**
     * The assumption here is that t1 is the argument of with a possible type parameter
     */
    public static Map<ClassType, Type> matchUpToTypeParameters(Type t1, Type t2, List<TypeVariable> typeParameters) {
        //Assuming the abstract type is t1
        Map<ClassType, Type> mapping = new HashMap<>();

        if (t1 instanceof ClassType && typeParameters.stream().anyMatch(tParam -> tParam.name().equals(((ClassType) t1).getName()))) {
            mapping.put((ClassType) t1, t2);
            return mapping;
        }

        if (t1 instanceof ClassType && t2 instanceof ClassType) {
            ClassType tt1 = (ClassType) t1;
            ClassType tt2 = (ClassType) t2;

            if (tt1.getFullName().equals(tt2.getFullName()) &&
                    tt1.params() != null && tt2.params() != null &&
                    tt1.params().length() == tt2.params().length()
            ) {
                for (int i = 0; i < tt1.params().length(); i++) {
                    Map<ClassType, Type> tmpMap = matchUpToTypeParameters((Type) tt1.params().apply(i), (Type) tt2.params().apply(i), typeParameters);
                    if (tmpMap == null || tmpMap.keySet().stream().anyMatch(mapping::containsKey)) {
                        return tmpMap;
                    }
                    mapping.putAll(tmpMap);
                }
            } else {
                return null;
            }
        } else if (t1 instanceof PrimitiveType && t2 instanceof PrimitiveType) {
            PrimitiveType tt1 = (PrimitiveType) t1;
            PrimitiveType tt2 = (PrimitiveType) t2;
            if (tt1.sort == tt2.sort && tt1.args().length() == tt2.args().length()) {
                for (int i = 0; i < tt1.args().size(); i++) {
                    Map<ClassType, Type> tmpMap = matchUpToTypeParameters((Type) tt1.argsJava().get(i), (Type) tt2.argsJava().get(i), typeParameters);
                    if (tmpMap == null || tmpMap.keySet().stream().anyMatch(mapping::containsKey)) {
                        // tmpMap == null or Conflicting mappings.
                        return tmpMap;
                    }
                    mapping.putAll(tmpMap);
                }
            } else {
                // Primitive types do not match
                return null;
            }
        }

        return mapping;
    }
}
