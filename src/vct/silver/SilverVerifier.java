package vct.silver;

import java.io.PrintWriter;
import java.util.List;
import java.util.Map;

public interface SilverVerifier<O,Err,T,E,S,Decl,P>
  extends SilverType<T>, SilverExpression<O,T,E, Decl >, SilverStatement<O,T,E, Decl, S> {

  /**
   * Get the origin of an object.
   * 
   * The result is undefined if the object does have an origin.
   * 
   * @param o Object with origin.
   * @return The origin of the object <code>o</code>.
   */
  public O getOrigin(Object o);
  
  /**
   * Create a (local) variable declaration.
   * 
   * @param o Origin
   * @param name Name
   * @param type Type
   * @return Declaration
   */
  public Decl decl(O o,String name,T type);
  
  /**
   * Create an empty program.
   * 
   * @return program
   */
  public P program();
  
  /**
   * Add a method to a program.
   * @param p Program
   * @param o Origin
   * @param name Method name
   * @param pres List of pre conditions
   * @param posts List of post conditions
   * @param in List of input argument declarations
   * @param out List of output argument declarations
   * @param local List of local variables
   * @param body Body statement
   */
  public void add_method(P p,O o,String name,
      List<E> pres,
      List<E> posts,
      List<Decl> in,
      List<Decl> out,
      List<Decl> local,
      S body);
  
  public void add_field(P p,O o,String name,T t);
  
  public void add_predicate(P p,O o,String name,List<Decl> args,E body);
  
  public void add_function(P p,O o,String name,List<Decl> args,T t,List<E> pres,List<E> posts,E body);
  
  /**
   * Verify a program.
   * @param tool The tool to be used for the verification.
   * @param program The program to be verified.
   * @return test report
   */
  public List<Err> verify(Object obj,String tool,P program);
  
  public void write_program(PrintWriter pw,P program);
}
