package fixbugs.test;

import java.sql.*;

public class JDBCTest {

    private final static String query = "SELECT name,tracks FROM cds";

	/**
	 * 
	 */
	public static void main(String[] args) throws java.sql.SQLException {
        //Class.forName("com.mysql.jdbc.Driver").newInstance();
        //String url = "jdbc:mysql://localhost/test";
        Connection conn = null; //DriverManager.getConnection(url, "cduser", "abc");
        conn.setAutoCommit(false);

        Statement st = conn.createStatement();
        int x = 1;
        /*try {
          ResultSet rs = st.executeQuery(query);
          while (rs.next()) {
            String s = rs.getString("name");
            int n = rs.getInt("tracks");
            System.out.println(s + " :  " + n);
          }
        }
        catch (SQLException ex) {
          System.err.println(ex.getMessage());
        }*/

        conn.commit();
	}

}
