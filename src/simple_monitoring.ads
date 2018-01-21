pragma Profile (Ravenscar);
pragma SPARK_Mode;

package simple_monitoring is

   function sign (val : in Float) return Float
      with Pre => val >= 10.0;

   task myTask;

end simple_monitoring;
