
import infodynamics.measures.continuous.kernel.MutualInfoCalculatorMultiVariateKernel;
import infodynamics.utils.ArrayFileReader;
import infodynamics.utils.MatrixUtils;

public class GeneratedCalculator {

    public static double b(String dataFile, int r) throws Exception {

        // 0. Load/prepare the data:
        ArrayFileReader afr = new ArrayFileReader(dataFile);
        double[][] data = afr.getDouble2DMatrix();
        double[] source = MatrixUtils.selectColumn(data, 0+r);
        double[] destination = MatrixUtils.selectColumn(data, 1+r);

        // 1. Construct the calculator:
        MutualInfoCalculatorMultiVariateKernel calc;
        calc = new MutualInfoCalculatorMultiVariateKernel();
        // 2. Set any properties to non-default values:
        // No properties were set to non-default values
        // 3. Initialise the calculator for (re-)use:
        calc.initialise();
        // 4. Supply the sample data:
        calc.setObservations(source, destination);
        // 5. Compute the estimate:
        double result = calc.computeAverageLocalOfObservations();
if (Double.isNaN(result)){
    return 0.0;

}else {
return result
        ;
    }
    }

    public static void main(String[] args) throws Exception {



        System.out.printf("{%.8f, %.8f,%.8f ,%.8f }",
                b( "output/firstNtimesteps.txt",0),
                b( "output/firstNtimesteps.txt",2),
                b( "output/lastNtimesteps.txt",0),
                b( "output/lastNtimesteps.txt",2));













        
            
            
            
            



  }
}
