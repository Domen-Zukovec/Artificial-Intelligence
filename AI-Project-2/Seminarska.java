import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;
import java.util.Stack;

public class Seminarska {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in).useDelimiter("\n");

        System.out.println("Vnesi p:");
        int p = Integer.parseInt(sc.next());
        System.out.println("Vnesi n:");
        int n = Integer.parseInt(sc.next());
        String[] temp = new String[n * p];
        System.out.print("Vpisi zacetno: ");
        System.out.println();
        Scanner sc_n, sc_v, sc_st;
        sc_v = new Scanner(System.in).useDelimiter("\n");

        String neki1, neki2;
        String temp1 = "";
        String temp2 = "";
        neki1 = sc_v.nextLine();
        sc_n = new Scanner(neki1);
        while (sc_n.hasNext()) {
            temp1 = temp1 + " " + sc_n.next();
        }
        temp1 = temp1.trim();

        temp = temp1.replaceAll("'", "").split(",");
        int[][] zacetna = napolni(temp, n, p);

        System.out.print("Vpisi koncno: ");
        System.out.println();

        neki2 = sc_v.nextLine();
        sc_st = new Scanner(neki2);
        while (sc_st.hasNext()) {
            temp2 = temp2 + " " + sc_st.next();
        }
        temp2 = temp2.trim();

        temp = temp2.replaceAll("'", "").split(",");
        int[][] koncna = napolni(temp, n, p);

        sc.close();
        sc_n.close();
        sc_st.close();
        sc_v.close();

        Node initNode = new Node(zacetna, n, p);
        long startTime = System.nanoTime();
        ArrayList<Node> solution = BFS(initNode, koncna);
        long endTime = System.nanoTime();
        for (int i = solution.size() - 1; i >= 0; i--) {
            if (solution.get(i).premik != null) {
                System.out.println("\n Izvedel se je premik: " + Arrays.toString(solution.get(i).premik));
            }
            System.out.println("***************");
            for (int[] row : solution.get(i).skatle) {
                System.out.println(Arrays.toString(row));
            }

        }

        System.out.println("\n Stevilo vozlisc: " + initNode.obstojece.size());
        System.out.println(" Potrebovan cas: " + (endTime - startTime) / 1000000 + "ms");
        System.out.println(" Globina rezultata: " + solution.get(0).depth);
        System.out.println(" Max globina: " + Global.max);
        System.out.println(" Stevilo premikov: " + (solution.size() - 1));

    }

    private static int[][] napolni(String[] input, int n, int p) {

        int[][] matrix = new int[n][p];
        for (int i = 0; i < input.length; i++) {
            if (!input[i].equals(" ")) {

                String z = input[i];
                char x = z.charAt(0);
                int ascii = (int) x;

                for (int f = 0; f < n; f++) {
                    for (int a = 0; a < p; a++) {
                        if (f * p + a == i) {
                            matrix[f][a] = ascii;
                        }
                    }
                }
            }
        }
        return matrix;
    }

    static class Node {

        private ArrayList<Node> otroci = new ArrayList<>();
        private ArrayList<int[][]> obstojece = new ArrayList<>();
        private int[] premik;
        private Node stars;
        private int n, p;
        private int[][] skatle;
        private int depth;

        public Node(int[][] input, int n, int p) {

            this.n = n;
            this.p = p;
            this.skatle = input;
            this.depth = 0;
        }

        public void expand() {
            int[][] temp = Arrays.stream(skatle).map(int[]::clone).toArray(int[][]::new);

            for (int i = 0; i < temp[0].length; i++) {
                for (int j = 0; j < temp[0].length; j++) {
                    prestavi(temp, i, j);
                    temp = Arrays.stream(skatle).map(int[]::clone).toArray(int[][]::new);
                }
            }

        }

        public void prestavi(int[][] a, int c, int r) {

            if (a[0][r] != 0 | c == r) {
                return;
            }
            int skatlaP = 0;
            boolean prazno = true;
            for (int i = 0; i < a.length; i++) {
                if (a[i][c] != 0) {
                    prazno = false;
                    skatlaP = a[i][c];
                    a[i][c] = 0;
                    break;
                }
            }
            if (!prazno) {
                prazno = true;
                for (int j = 0; j < a.length; j++) {
                    if (a[j][r] != 0) {
                        prazno = false;
                        a[j - 1][r] = skatlaP;
                        break;
                    }

                }
                if (prazno) {
                    a[a.length - 1][r] = skatlaP;
                }
            }
            boolean obstaja = false;
            for (int[][] x : obstojece) {
                if (Arrays.deepEquals(x, a) | Arrays.deepEquals(skatle, a)) {
                    obstaja = true;
                    break;
                }
            }
            if (!obstaja) {
                Node otrok = new Node(a, n, p);
                obstojece.add(otrok.skatle);
                otroci.add(otrok);
                otrok.stars = this;
                otrok.depth = otrok.stars.depth + 1;
                otrok.obstojece = this.obstojece;
                otrok.premik = new int[] { c, r };
            }

        }

        public boolean isKoncna(int[][] koncna, int[][] curr) {

            return Arrays.deepEquals(koncna, curr);

        }
    }

    public static ArrayList<Node> BFS(Node root, int[][] koncna) {
        ArrayList<Node> PathToSolution = new ArrayList<>();
        ArrayList<Node> OpenList = new ArrayList<>();
        ArrayList<Node> ClosedList = new ArrayList<>();

        OpenList.add(root);
        boolean goalFound = false;
        while (!OpenList.isEmpty() & !goalFound) {
            Node currentNode = OpenList.get(0);

            ClosedList.add(currentNode);
            OpenList.remove(0);

            currentNode.expand();
            for (int i = 0; i < currentNode.otroci.size(); i++) {
                Node currentChild = currentNode.otroci.get(i);
                if (Global.max < currentChild.depth) {
                    Global.max = currentChild.depth;
                }
                if (currentChild.isKoncna(koncna, currentChild.skatle)) {
                    goalFound = true;
                    PathTrace(PathToSolution, currentChild);
                }
                if (!contains(OpenList, currentChild) & !contains(ClosedList, currentChild)) {
                    OpenList.add(currentChild);
                }
            }
        }

        return PathToSolution;
    }

    public static ArrayList<Node> DFS(Node root, int[][] koncna) {
        ArrayList<Node> PathToSolution = new ArrayList<>();
        Stack<Node> OpenList = new Stack<>();
        ArrayList<Node> ClosedList = new ArrayList<>();
        ClosedList.add(root);
        OpenList.push(root);
        boolean goalFound = false;
        while (!OpenList.isEmpty() & !goalFound) {
            Node currentNode = OpenList.peek();
            if (Global.max < currentNode.depth) {
                Global.max = currentNode.depth;
            }
            if (currentNode.isKoncna(koncna, currentNode.skatle)) {
                goalFound = true;
                PathTrace(PathToSolution, currentNode);
            }

            currentNode.expand();
            boolean found = false;
            for (int nextChild = 0; nextChild < currentNode.otroci.size(); nextChild++) {
                if (currentNode.otroci.get(nextChild) != null
                        & !contains(ClosedList, currentNode.otroci.get(nextChild))) {
                    ClosedList.add(currentNode.otroci.get(nextChild));
                    OpenList.push(currentNode.otroci.get(nextChild));
                    found = true;
                    break;
                }
            }
            if (!found) {
                OpenList.pop();
            }
        }

        return PathToSolution;
    }

    public static ArrayList<Node> DFS2(Node root, int[][] koncna) {
        ArrayList<Node> PathToSolution = new ArrayList<>();
        ArrayList<Node> NeGlej = new ArrayList<>();

        Node currNode = root;
        boolean goalFound = false;
        while (!goalFound) {

            if (currNode.isKoncna(koncna, currNode.skatle)) {
                goalFound = true;
                PathTrace(PathToSolution, currNode);
            }

            currNode.expand();
            boolean tf = false;
            for (int j = 0; j < currNode.otroci.size(); j++) {
                if (!contains(NeGlej, currNode.otroci.get(j))) {
                    tf = true;
                    break;
                }
            }
            if (!tf) {
                NeGlej.add(currNode);
                currNode = currNode.stars;
            } else {
                for (int i = 0; i < currNode.otroci.size(); i++) {
                    if (Global.max < currNode.otroci.get(i).depth) {
                        Global.max = currNode.otroci.get(i).depth;
                    }
                    if (!contains(NeGlej, currNode.otroci.get(i))) {
                        currNode = currNode.otroci.get(i);
                        break;
                    }
                }
            }
        }
        return PathToSolution;
    }

    public static ArrayList<Node> IDDFS(Node root, int[][] koncna) {
        ArrayList<Node> PathToSolution = new ArrayList<>();
        Stack<Node> OpenList = new Stack<>();
        ArrayList<Node> ClosedList = new ArrayList<>();
        boolean goalFound = false;
        int depth = 0;

        while (!goalFound) {

            ClosedList.add(root);
            OpenList.push(root);

            System.out.println(depth);
            while (!OpenList.isEmpty()) {
                Node currentNode = OpenList.peek();
                if (Global.max < currentNode.depth) {
                    Global.max = currentNode.depth;
                }
                if (currentNode.isKoncna(koncna, currentNode.skatle)) {
                    goalFound = true;
                    PathTrace(PathToSolution, currentNode);
                    break;

                }

                boolean found = false;
                if (currentNode.depth < depth) {

                    currentNode.expand();

                    for (int nextChild = 0; nextChild < currentNode.otroci.size(); nextChild++) {
                        if (currentNode.otroci.get(nextChild) != null
                                & !contains(ClosedList, currentNode.otroci.get(nextChild))) {
                            ClosedList.add(currentNode.otroci.get(nextChild));
                            OpenList.push(currentNode.otroci.get(nextChild));
                            found = true;
                            break;
                        }
                    }
                }
                if (!found) {
                    OpenList.pop();
                }

            }
            ClosedList.clear();
            depth++;
        }

        return PathToSolution;
    }

/*
    public static ArrayList<Node> IDDFS2(Node root, int[][] koncna)
    {
        ArrayList<Node> PathToSolution = new ArrayList<>();
        ArrayList<Node> NeGlej = new ArrayList<>();

        Node currNode = root;

        boolean goalFound = false;
        int ct = 0;
        int gl = 1;
        int pregledani = 0;
        int koliko = prestej(currNode, gl - 1);
        while(!goalFound){

            if(koliko == pregledani){
                gl++;
                pregledani=0;
                ct=0;
                NeGlej.clear();
                currNode = root;
                koliko = prestej(currNode, gl -1);
            }
            if(currNode.isKoncna(koncna,currNode.skatle)) {
                goalFound = true;
                PathTrace(PathToSolution,currNode);
            }


            currNode.expand();
            ct++;
            boolean tf = false;
            for(int j = 0; j<currNode.otroci.size();j++) {
                if(!contains(NeGlej,currNode.otroci.get(j))) {
                    tf = true;
                    break;
                }
            }
            if(!tf || ct == gl) {
                NeGlej.add(currNode);
                pregledani++;
                currNode = currNode.stars;
                ct -= 2;
            } else {
                for(int i = 0; i<currNode.otroci.size();i++) {
                    if(!contains(NeGlej,currNode.otroci.get(i))) {
                        currNode = currNode.otroci.get(i);
                        break;
                    }
                }
            }
        }
        return PathToSolution;
    }

    public static int prestej(Node root, int gl)
    {
        ArrayList<Node> OpenList = new ArrayList<>();
        ArrayList<Node> ClosedList = new ArrayList<>();

        OpenList.add(root);
        int globina = gl;
        int ct = 0;
        int stevilo = 0;
        while(!OpenList.isEmpty() && ct < gl)
        {
            Node currentNode = OpenList.get(0);
            ClosedList.add(currentNode);
            OpenList.remove(0);

            currentNode.expand();
            ct++;
            for(int i = 0; i<currentNode.otroci.size();i++){
                Node currentChild = currentNode.otroci.get(i);
                if(!contains(OpenList,currentChild) && !contains(ClosedList,currentChild))
                {
                    OpenList.add(currentChild);
                    stevilo++;
                }
            }
        }
        stevilo++;
        return stevilo;
    }
*/

    public static void PathTrace(ArrayList<Node> path, Node c) {

        Node curr = c;
        path.add(curr);
        while (curr.stars != null) {
            curr = curr.stars;
            path.add(curr);
        }

    }

    public static boolean contains(ArrayList<Node> list, Node c) {
        for (Node f : list) {
            if (Arrays.deepEquals(f.skatle, c.skatle)) {
                return true;
            }
        }
        return false;
    }

    public static class Global {
        public static int max = 0;
    }
}
