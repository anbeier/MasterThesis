package yichi;

public class Configuration {
  public String graph_file;
  public Double min_deg_ratio;
  public Integer min_size;
  public Integer nmax_size;
  public String output_file;
  
  public Configuration(
	  String graph_file,
	  Double min_deg_ratio,
	  Integer min_size,
	  Integer nmax_size,
	  String output_file
  ) {
    this.graph_file = graph_file;
    this.min_deg_ratio = min_deg_ratio;
    this.min_size = min_size;
    this.nmax_size = nmax_size;
    this.output_file = output_file;
  }
  
  public static Configuration fromArgs(String[] args) {
    String graph_file = args[0];
    Double min_deg_ration = Double.valueOf(args[1]);
    Integer min_size = Integer.valueOf(args[2]);
    Integer nmax_size = Integer.valueOf(args[3]);
    String output_file = args[4];
    
    return new Configuration(
	    graph_file, 
	    min_deg_ration, 
	    min_size, 
	    nmax_size, 
	    output_file
    );
  }
}
