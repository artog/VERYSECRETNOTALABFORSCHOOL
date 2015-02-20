package edu.gu.hajo.mbclient.core.state;

/**
 * Just to reduce dependencies
 * @author hajo
 *
 */
public class StateFactory {

   public static StateContext getStateContext(){
       return new StateContext();
   }

}
