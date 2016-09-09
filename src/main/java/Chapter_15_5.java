import java.io.IOException;

public class Chapter_15_5 {
    public static void main(String[] args) {
        try {
            System.out.println(Chapter_15.e5("123"));
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}
