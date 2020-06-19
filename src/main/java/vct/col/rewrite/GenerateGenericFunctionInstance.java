package vct.col.rewrite;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

import java.util.Map;

public class GenerateGenericFunctionInstance extends AbstractRewriter {

    private Method m;
    private Map<ClassType, Type> mapping;

    public GenerateGenericFunctionInstance() {
        super(null, null);
    }

    public Type rewrite(Type t, Map<ClassType, Type> mapping) {
        this.mapping = mapping;
        return rewrite(t);
    }


    public Method rewrite(Method m, Map<ClassType, Type> mapping) {
        this.m = m;
        this.mapping = mapping;

        DeclarationStatement[] args = rewrite(m.getArgs());
        Type returnType = rewrite(m.getReturnType());

        StringBuilder generated_name = new StringBuilder(m.getName());
        for (DeclarationStatement typeParam: args) {
            generated_name.append("_").append(typeParam.getType().toString());
        }
        generated_name.append("_").append(returnType.toString());

        Contract contract=m.getContract();
        if (currentContractBuilder==null){
            currentContractBuilder=new ContractBuilder();
        }
        if (contract!=null){
            rewrite(contract,currentContractBuilder);
        }

        Method res = new Method(m.kind, generated_name.toString(), returnType, currentContractBuilder.getContract(),args,m.usesVarArgs(),rewrite(m.getBody()));
        res.copyMissingFlags(m);
        res.setOrigin(m.getOrigin());
        return res;
    }

    @Override
    public void visit(DeclarationStatement s) {
        DeclarationStatement res=new DeclarationStatement(s.name(),rewrite(s.getType()),s.initJava());
        result = res;
    }


    @Override
    public void visit(MethodInvokation e) {
        if (m.getName().equals(e.method)) {

            StringBuilder generatedName = new StringBuilder(e.method);
            if (e.getArgs() != null && e.getArgs().length > 0) {
                for (ASTNode arg : e.getArgs()) {
                    generatedName.append("_").append(rewrite(arg.getType()).toString());
                }
            }
            generatedName.append("_").append(rewrite(e.getType()).toString());

            result = create.invokation(rewrite(e.object), rewrite(e.dispatch), generatedName.toString(), rewrite(e.getArgs()));
        } else {
            super.visit(e);
        }
    }

    @Override
    public void visit(ClassType t) {
        result = mapping.get(t);
    }

}
