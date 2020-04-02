package vct.col.util;

import scala.collection.JavaConverters;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.ast.util.RecursiveVisitor;

import java.util.*;
import java.util.stream.Collectors;

public class GenericsScanner extends RecursiveVisitor<Object> {

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
    public void visit(MethodInvokation e) {
        if (e.dispatch != null) {
            List<Type> typeParams = JavaConverters.seqAsJavaList(e.dispatch.params()).stream().map(t -> (Type) t).collect(Collectors.toList());
            Set<List<Type>> mappings = this.mappings.getOrDefault(e.dispatch, new HashSet<>());
            mappings.add(typeParams);
            this.mappings.put(e.dispatch, mappings);
        } else if (e.object != null && e.object instanceof NameExpression && source().find(((NameExpression) e.object).getName()) != null && source().find(((NameExpression) e.object).getName()).find_predicate(e.method) != null) {
            staticMethodInvokations.add(e);
        } else {
            super.visit(e);
        }
    }

    public Map<ClassType, Set<List<Type>>> getMappings() {
        return mappings;
    }

    public List<MethodInvokation> getStaticMethodInvokations() {
        return staticMethodInvokations;
    }
}

