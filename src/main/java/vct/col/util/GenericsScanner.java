package vct.col.util;

import scala.collection.JavaConverters;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.ast.util.RecursiveVisitor;

import java.util.*;
import java.util.stream.Collectors;

public class GenericsScanner extends RecursiveVisitor<Object> {

    private Map<ClassType, List<List<Type>>> mappings = new HashMap<>();

    public GenericsScanner() {
        super(null, null);
    }

    @Override
    public void visit(ASTClass c) {
        if (c.kind == ASTClass.ClassKind.Abstract && c.parameters != null && c.parameters.length > 0){
            return;
        }
        super.visit(c);
    }

    @Override
    public void visit(MethodInvokation e) {

        List<Type> typeParams = JavaConverters.seqAsJavaList(e.dispatch.params()).stream().map(t -> (Type) t).collect(Collectors.toList());
        List<List<Type>> mappings = this.mappings.getOrDefault(e.dispatch, new ArrayList<>());
        mappings.add(typeParams);
        this.mappings.put(e.dispatch, mappings);
    }

    public Map<ClassType, List<List<Type>>> getMappings() {
        return mappings;
    }


}

