package yichi;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Scanner;
import edu.uci.ics.jung.graph.*;


public class GraphGenerator {
		
	public String file;

	public Graph<Integer, Edge> createGraph(String filePath) throws FileNotFoundException {
		
		this.file = filePath;
		Graph<Integer, Edge> g = new UndirectedSparseGraph<Integer, Edge>() {};
		
		//to calculate how many vertices there are in the graph. It refers to the number of rows in the file.
		//Vertices are identified with integers starting from 0.
		Scanner scanRows = new Scanner(new FileReader(this.file));
		int row = -1;
		while (scanRows.hasNextLine()){
			row++;  
			g.addVertex(row);			
			String line = scanRows.nextLine();
			//to add neighbors of this vertex.
			@SuppressWarnings("resource")
			Scanner scanALine = new Scanner(line).useDelimiter(" ");			
			while (scanALine.hasNext()) {
				int v = scanALine.nextInt();
                                Edge e = new Edge(row, v);
                                if (!g.containsEdge(e)) {
                                  g.addEdge(e, row, v);
                                }
			}
//			scanALine.close();
		}	
		scanRows.close();
		
		return g;		
	}
	
	public int size() throws FileNotFoundException {
		
		Scanner s = new Scanner(new FileReader(this.file));
		int count = 0;
		while (s.hasNextLine()){
			count++;  
			s.nextLine();
		}
		s.close();
		return count;
	}
	
}
