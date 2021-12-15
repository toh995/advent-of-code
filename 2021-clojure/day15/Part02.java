import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.HashSet;
import java.util.PriorityQueue;

public class Part02 {
    public static void main(String[] args) {
        int[][] matrix = GetMatrix.getMatrix();
        Coordinate startCoord = new Coordinate(0, 0);
        Coordinate targetCoord = new Coordinate(matrix.length-1, matrix[0].length-1);

        double finalAnswer = Dijkstra.run(matrix, startCoord, targetCoord);

        System.out.println(finalAnswer);
    }
}

class Dijkstra {
    public static double run(int[][] matrix, Coordinate startCoord, Coordinate targetCoord) {
        int initialCapacity = matrix.length * matrix[0].length;

        Comparator<Coordinate> comparator = new CoordinateComparator();
        PriorityQueue<Coordinate> queue = new PriorityQueue<Coordinate>(initialCapacity, comparator);
        
        Coordinate[][] coordMap = new Coordinate[matrix.length][matrix[0].length];
        
        // populate the queue and coordMap now
        for (int i=0; i<matrix.length; i++) {
            for (int j=0; j<matrix[0].length; j++) {

                if (i==startCoord.i && j==startCoord.j) {
                    startCoord.setDistance(0);
                    queue.add(startCoord);
                    coordMap[i][j] = startCoord;
                }
                else if (i==targetCoord.i && j==targetCoord.j) {
                    queue.add(targetCoord);
                    coordMap[i][j] = targetCoord;
                }
                else {
                    Coordinate newCoord = new Coordinate(i, j);
                    queue.add(newCoord);
                    coordMap[i][j] = newCoord;
                }
            }
        }

        // commence the algorithm now...
        while (queue.size() > 0) {
            Coordinate currentCoord = queue.remove();

            // return early if we have reached the targetCoord
            if (currentCoord.i == targetCoord.i && currentCoord.j == targetCoord.j) {
                return currentCoord.getDistance();
            }

            Coordinate[] neighbors = getNeighbors(matrix, coordMap, currentCoord);

            for (int i=0; i<neighbors.length; i++) {
                Coordinate neighbor = neighbors[i];
                double newDistance = currentCoord.getDistance() + matrix[neighbor.i][neighbor.j];
                
                if (newDistance < neighbor.getDistance()) {
                    queue.remove(neighbor);
                    neighbor.setDistance(newDistance);
                    queue.add(neighbor);
                }
            }
        }

        return -1;
    }

    public static Coordinate[] getNeighbors(int[][] matrix, Coordinate[][] coordMap, Coordinate currentCoord) {
        int i = currentCoord.i;
        int j = currentCoord.j;

        HashSet<Coordinate> neighbors = new HashSet<Coordinate>();
        int[][] deltas = {{0,1}, {0,-1}, {1,0}, {-1,0}};

        for (int k=0; k<deltas.length; k++) {
            int[] delta = deltas[k];
            int iNew = i + delta[0];
            int jNew = j + delta[1];

            if (0<=iNew && iNew<matrix.length) {
                if (0<=jNew && jNew<matrix[0].length) {
                    Coordinate neighbor = coordMap[iNew][jNew];
                    neighbors.add(neighbor);
                }
            }
        }
        
        return neighbors.toArray(new Coordinate[neighbors.size()]);
    }
}

class Coordinate {
    public final int i;
    public final int j;
    private double distance;

    Coordinate(int i, int j) {
        this.i = i;
        this.j = j;
        this.distance = Double.POSITIVE_INFINITY;
    }

    public double getDistance() {
        return distance;
    }

    public double setDistance(double distance) {
        this.distance = distance;
        return distance;
    }
}

class CoordinateComparator implements Comparator<Coordinate> {
    @Override
    public int compare(Coordinate c1, Coordinate c2) {
        if (c1.getDistance() < c2.getDistance()) {
            return -1;
        }
        if (c1.getDistance() > c2.getDistance()) {
            return 1;
        }
        return 0;
    }
}

class GetMatrix {
    public static int[][] getMatrix() {
        int[][] matrix = getOriginalMatrix();        
        matrix = expandDown(5, matrix);

        // extend rows
        for (int i=0; i<matrix.length; i++) {
            matrix[i] = extendRow(5, matrix[i]);
        }

        return matrix;
    }
    
    public static int[][] getOriginalMatrix() {
        String fileString = readFile();
        String[] lines = fileString.split("\n");

        int[][] matrix = new int[lines.length][lines[0].length()];

        for (int i=0; i<lines.length; i++) {
            String[] row = lines[i].split("");

            for (int j=0; j<row.length; j++) {
                matrix[i][j] = Integer.parseInt(row[j]);
            }
        }
        return matrix;
    }

    public static String readFile() {
        String fileString = null;
        try{
            fileString = Files.readString(Paths.get("./data.txt"));
        }
        catch (IOException e) {
            e.printStackTrace();
        }

        return fileString;
    }

    public static int incrementCell(int cellVal) {
        return (cellVal == 9) ? 1 : (cellVal + 1);
    }

    public static int[] incrementRow(int[] row) {
        int[] ret = new int[row.length];

        for (int i=0; i<row.length; i++) {
            ret[i] = incrementCell(row[i]);
        }

        return ret;
    }

    public static int[] extendRow(int factor, int[] row) {
        int[] ret = new int[row.length*factor];

        // first copy row into ret
        for (int i=0; i<row.length; i++) {
            ret[i] = row[i];
        }
        
        // now fill out the rest of ret
        for (int i=row.length; i<ret.length; i++) {
            ret[i] = incrementCell(ret[i-row.length]);
        }

        return ret;
    }

    public static int[][] expandDown(int factor, int[][] matrix) {
        int rowLength = matrix[0].length;

        int[][] ret = new int[matrix.length*factor][rowLength];

        // first copy the matrix into ret
        for (int i=0; i<matrix.length; i++) {
            ret[i] = matrix[i];
        }

        // now fill out the rest of ret
        for (int i=matrix.length; i<ret.length; i++) {
            ret[i] = incrementRow(ret[i-matrix.length]);
        }

        return ret;
    }
}
