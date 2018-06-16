package rs.ac.bg.etf.zd173013m;

import javax.swing.*;
import java.awt.*;

public class App {

    private JButton buttonMsg;
    private JPanel panelMain;

    public static void main(String[] args) {
        JFrame frame = new JFrame("App");
        App app = new App();
        frame.setContentPane(app.panelMain);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
        Component component = app.panelMain.getComponent(0);
        JButton button = (JButton)component;


        Test test = new Test();
        button.setText(test.nesto());

    }
}
