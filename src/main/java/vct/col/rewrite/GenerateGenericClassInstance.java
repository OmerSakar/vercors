package vct.col.rewrite;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.ast.type.TypeVariable;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

import java.util.List;
import java.util.OptionalInt;
import java.util.stream.IntStream;

public class GenerateGenericClassInstance extends AbstractRewriter {

    private ASTClass c;
    private List<Type> mapping;
    private String generatedClassName = "";

    public GenerateGenericClassInstance() {
        super(null, null);
    }

    public ASTClass rewrite(ASTClass c, List<Type> mapping) {
        this.c = c;
        this.mapping = mapping;

        if (c == null || c.parameters == null || c.parameters.length != mapping.size()) {
            //TODO a better error message with origin
            Fail("The types in the method invokation or constructor does not match the generic type.");
        }

        StringBuilder generatedName = new StringBuilder(c.getName());

        for (Type type: mapping) {
            generatedName.append("_").append(type.toString());
        }
        this.generatedClassName = generatedName.toString();

        ASTClass res = new ASTClass(generatedClassName, ASTClass.ClassKind.Plain,new DeclarationStatement[0],rewrite(c.super_classes),rewrite(c.implemented_classes));

        res.setOrigin(c.getOrigin());

        Contract contract=c.getContract();
        if (currentContractBuilder==null){
            currentContractBuilder=new ContractBuilder();
        }
        if (contract!=null){
            rewrite(contract,currentContractBuilder);
        }
        res.setContract(currentContractBuilder.getContract());
//        currentContractBuilder=null;
        for(ASTNode item:c){
            res.add(rewrite(item));
        }
        result = res;
        return (ASTClass) result;
    }

    @Override
    public void visit(DeclarationStatement s) {
//        super.visit(s);
        DeclarationStatement res=new DeclarationStatement(s.name(),rewrite(s.getType()),s.initJava());
        result = res;
    }

    @Override
    public void visit(ClassType t) {
//        super.visit(t);
        OptionalInt indexOpt = IntStream.range(0, c.parameters.length)
                .filter(i -> ((TypeVariable) c.parameters[i].getType()).name().equals(t.getFullName()))
                .findFirst();

        if (!indexOpt.isPresent()) {
            Fail("Type parameter %s not found in class %s", t, c.getName());
        }

        Type mappedType = this.mapping.get(indexOpt.getAsInt());

        result = mappedType;
    }

    @Override
    public void visit(Method m) {
//        super.visit(m);
        if (m.kind == Method.Kind.Constructor) {
            result=create.method_kind(
                    m.kind,
                    rewrite(m.getReturnType()),
                    rewrite(m.getContract()),
                    this.generatedClassName,
                    rewrite(m.getArgs()),
                    rewrite(m.getBody())
            );
        } else if (m.kind != Method.Kind.Pure) {
            Fail("Methods in generic classes are not supported.");
        } else {
            super.visit(m);
        }
    }
}
