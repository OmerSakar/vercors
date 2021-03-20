package vct.silver;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import static hre.lang.System.Debug;
import static hre.lang.System.Progress;

import vct.logging.ExceptionMessage;
import vct.logging.MessageVisitor;
import vct.logging.TaskBegin;
import vct.logging.TaskEnd;
import vct.logging.TaskPhase;
import vct.logging.VerCorsError;
import vct.logging.VerificationResult;

public class ErrorDisplayVisitor implements MessageVisitor {

  @Override
  public void visit(ExceptionMessage e) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(TaskBegin begin) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(TaskEnd end) {
    long duration=(end.nanoTime()-end.begin.nanoTime())/1000000L;
    if(duration>1L) {
      Progress("task %s took %d ms",end.begin.description,duration);
    }

    
  }

  @Override
  public void visit(TaskPhase phase) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(VerificationResult result) {
    // TODO Auto-generated method stub
    
  }

  @Override
  public void visit(VerCorsError error) {
    Debug("reporting %s error",error.code);
    if (error.main == null) {
      new MessageOrigin("Missing origin").report("error","%s:%s",error.code,error.sub);
    } else {
      error.main.report("error","%s:%s",error.code,error.sub);
    }
    for(Origin o:error.aux){
      o.report("auxiliary","caused by");
    }
  }

}
