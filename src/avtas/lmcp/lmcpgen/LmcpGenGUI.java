// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

/*
 * LmcpGenGUI.java
 *
 * Created on August 7, 2007, 7:40 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package avtas.lmcp.lmcpgen;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.CopyOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.FileAttribute;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.swing.AbstractAction;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.UIManager;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 *
 * @author Matt Duquette AFRL/VACD
 */
public class LmcpGenGUI {

    JFrame frame = null;
    JTextArea textArea = new JTextArea(10, 40);
    JButton goButton = new JButton("Create Package");
    FilePanel outputField = new FilePanel("Select");
    MultiFilePanel mdmFile = new MultiFilePanel("Add MDM", "Remove MDM");
    FilePanel classField = new FilePanel("Select");
    FilePanel templateField = new FilePanel("Select");
    JPanel checkPanel = new JPanel();
    JCheckBox javaBox = new JCheckBox("Java");
    JCheckBox cppBox = new JCheckBox("C++");
    JCheckBox csBox = new JCheckBox("C#");
    JCheckBox htmlBox = new JCheckBox("Docs");
    JCheckBox pythonBox = new JCheckBox("Python");
    JCheckBox adaBox = new JCheckBox("Ada");
    JCheckBox customBox = new JCheckBox("Custom Language");
    MDMInfo[] infos = null;
    File outputDir = null;
    File template = null;
    Class runClass = null;
    ArrayList<PackageInfo> packageList = new ArrayList<PackageInfo>();
    File lastDirectory = new File(".");

    /** Creates a new instance of LmcpGenGUI */
    public LmcpGenGUI() {
        try {
            frame = new JFrame("LmcpGen");
            outputField.setBorder(new TitledBorder("Output Directory"));
            mdmFile.setBorder(new TitledBorder("MDM File"));
            classField.setBorder(new TitledBorder("Methods File"));
            classField.setEnabled(false);
            templateField.setBorder(new TitledBorder("Template File"));
            templateField.setEnabled(false);
            JPanel p = new JPanel();
            frame.add(p);
            p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
            outputField.getChooser().setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            classField.getChooser().setFileFilter(new FileNameExtensionFilter("Class Files", "class", "java"));
            templateField.getChooser().setFileFilter(new FileNameExtensionFilter("Template Files", "tl"));
            mdmFile.getChooser().setFileFilter(new FileNameExtensionFilter("Template Files", "xml"));
            checkPanel.setLayout(new GridLayout(0, 3));
            checkPanel.setBorder(new TitledBorder("Choose Languages"));

            URL optionsFile = getClass().getResource("/avtas/lmcp/lmcpgen/gui_setup.xml");
            if (optionsFile != null) {
                Element el = XMLUtil.parse(optionsFile.openStream());
                //frame.setTitle(frame.getTitle() + "  " + XMLUtil.get(el, "Version", ""));
                Node[] nodes = XMLUtil.getList(el, "PackageList", "Package");
                for (Node n : nodes) {
                    if (n instanceof Element) {
                        addPackage(XMLUtil.getAttribute(n, "DisplayName", ""),
                                XMLUtil.getAttribute(n, "TemplateListing", ""),
                                XMLUtil.getAttribute(n, "MethodsClass", ""),
                                XMLUtil.getAttribute(n, "SubFolder", ""));
                    }
                }
            }

            final JToggleButton selectAllBut = new JToggleButton("Select All");
            selectAllBut.addActionListener(new AbstractAction() {
                public void actionPerformed(ActionEvent e) {
                    for (PackageInfo pi : packageList ) {
                        pi.checkBox.setSelected(selectAllBut.isSelected());
                        if (selectAllBut.isSelected()) {
                            selectAllBut.setText("Select None");
                        }
                        else {
                            selectAllBut.setText("Select All");
                        }
                    }
                }
            });
            checkPanel.add(selectAllBut);


            JMenuBar menuBar = new JMenuBar();
            frame.setJMenuBar(menuBar);
            JMenu helpMenu = menuBar.add(new JMenu("Help"));
            helpMenu.add(new AbstractAction("About LMCP") {

                public void actionPerformed(ActionEvent e) {
                    try {
                        showHelpWin("/doc/lmcp.html");
                    } catch (Exception ex) {
                        Logger.getLogger(LmcpGenGUI.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            });
            helpMenu.add(new AbstractAction("Using LmcpGen") {

                public void actionPerformed(ActionEvent e) {
                    showHelpWin("/doc/lmcpgen.html");
                }
            });
            helpMenu.add(new AbstractAction("Show Change Log") {

                public void actionPerformed(ActionEvent e) {
                    showHelpWin("/avtas/lmcp/lmcpgen/ChangeLog.html");
                }
            });
            helpMenu.add(new AbstractAction("Unpack LmcpGen Source") {

                public void actionPerformed(ActionEvent e) {
                    unpackSourceJar();
                }
            });

            p.add(outputField);

            p.add(mdmFile);

            p.add(checkPanel);

            JPanel tmp = new JPanel(new BorderLayout());
            tmp.add(customBox, BorderLayout.WEST);
            p.add(tmp);
            p.add(templateField);
            p.add(classField);

            p.add(new JScrollPane(textArea));
            goButton.setAlignmentX(0.5f);
            p.add(goButton);
            goButton.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    runLmcpGen();
                }
            });
            customBox.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    classField.setEnabled(customBox.isSelected());
                    templateField.setEnabled(customBox.isSelected());
                }
            });
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.pack();
            frame.setResizable(false);
            frame.setVisible(true);

        } catch (IOException ex) {
            Logger.getLogger(LmcpGenGUI.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    public static void main(String[] args) {
        if (args.length > 0) {
            LmcpGen.main(args);
        }
        else {

            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } catch (Exception ex) {
                ex.printStackTrace();
            }

            new LmcpGenGUI();
        }
    }

    void runLmcpGen() {

        new Thread() {

            @Override
            public void run() {

                try {
                    // read the MDM files
                    textArea.append("Parsing the MDM...");
                    String[] mdmPaths = mdmFile.getPaths();
                    infos = new MDMInfo[mdmPaths.length];
                    for (int i = 0; i < mdmPaths.length; i++) {
                        MDMInfo info = MDMReader.readMDM(new File(mdmPaths[i]));
                        if (info == null) {
                            throw new Exception("Bad MDM File Specified for " + mdmPaths[i]);
                        }
                        infos[i] = info;
                    }

                    textArea.append("Done.\n");

                    // set up the output directory
                    outputDir = new File(outputField.getPath());
                    if (!outputDir.isDirectory()) {
                        outputDir = null;
                        textArea.append("Output Directory is not a Directory.\n");
                        throw new Exception("Output Directory Error");
                    }

                    // create the packages
                    textArea.append("Trying to create packages...\n");
                    if (infos == null || infos.length == 0) {
                        throw new Exception("No MDM File Specified");
                    }
                    if (outputDir == null) {
                        textArea.append("You must specify an Output Directory.\nDone.\n");
                        throw new Exception("No Output Directory Specified");
                    }

                    // write out the MDMs into the main output directory
                    LmcpGen.writeMDMs(infos, outputDir);

                    if (customBox.isSelected()) {
                        template = new File(templateField.getPath());
                        if (!template.exists()) {
                            template = null;
                            textArea.append("Template file does not exist.\n");
                            throw new Exception("Template File Error");
                        }
                        // read in the class file or class name
                        File cFile = new File(classField.getPath());
                        if (cFile.getName().endsWith(".class")) {
                            URLClassLoader loader = URLClassLoader.newInstance(new URL[]{cFile.getParentFile().toURI().toURL()});
                            runClass = loader.loadClass(cFile.getName().substring(0, cFile.getName().indexOf(".class")));
                            textArea.append("Loaded a method file named..." + runClass + "\n");
                        }
                        else {
                            runClass = ClassLoader.getSystemClassLoader().loadClass(classField.getPath());
                            textArea.append("Loaded a method class named..." + runClass + "\n");
                        }
                        if (runClass != null && template != null) {
                            textArea.append("Creating custom code at..." + outputField.getPath() + "/custom \n");
                            LmcpGen.makePackage(infos,
                                    new File(outputDir, "custom"),
                                    new File(templateField.getPath()).toURI().toURL(),
                                    runClass);
                            textArea.append("Success.\n");
                        }
                    }

                    for (PackageInfo set : packageList) {
                        if (set.checkBox.isSelected()) {
                            textArea.append("Creating " + set.displayName + " code at..." + outputField.getPath() + "/" + set.outputDir + "\n");
                            LmcpGen.makePackage(infos, new File(outputDir, set.outputDir), getClass().getResource(set.templateListing),
                                    Class.forName(set.methodsClass));
                            textArea.append("Done.\n");
                        }
                    }

                    textArea.append("Creation Success\n");

                } catch (Exception ex) {
                    ex.printStackTrace();
                    JOptionPane.showMessageDialog(frame, ex.getMessage());
                }

            }
        }.start();

    }

    void addPackage(String displayName, String templateListing, String methodsClass, String outputDir) {

        PackageInfo set = new PackageInfo(displayName, templateListing, methodsClass, outputDir);
        packageList.add(set);
        checkPanel.add(set.checkBox);
    }

    class FilePanel extends JPanel {

        JFileChooser chooser = new JFileChooser();
        JButton but = new JButton();
        JTextField field = new JTextField(40);

        public FilePanel(String buttonText) {
            but.setText(buttonText);
            but.setActionCommand(buttonText);
            chooser.setMultiSelectionEnabled(false);

            but.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    chooser.setCurrentDirectory(lastDirectory);
                    int res = chooser.showDialog(but.getParent(), "Select");
                    if (res == JFileChooser.APPROVE_OPTION) {
                        field.setText(chooser.getSelectedFile().getAbsolutePath());
                        lastDirectory = chooser.getSelectedFile().getParentFile();
                    }
                }
            });

            add(field);
            add(but);
        }

        public JFileChooser getChooser() {
            return chooser;
        }

        public String getPath() {
            return field.getText();
        }

        @Override
        public void setEnabled(boolean enabled) {
            super.setEnabled(enabled);
            for (Component c : getComponents()) {
                c.setEnabled(enabled);
            }
        }
    }

    void showHelpWin(String url) {
        try {

            JEditorPane pane = new JEditorPane(getClass().getResource(url));
            JFrame f = new JFrame();
            f.add(new JScrollPane(pane));
            f.setSize(480, 640);
            f.setLocationRelativeTo(frame);
            f.setVisible(true);
        } catch (Exception ex) {
            Logger.getLogger(LmcpGenGUI.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /** Unpacks the LmcpGen Source into a designated folder. */
    public void unpackSourceJar() {

        CodeSource cs = this.getClass().getProtectionDomain().getCodeSource();

        URL loc = cs.getLocation();

        try {
            if (loc.getProtocol().equals("file") && loc.getPath().toLowerCase().endsWith(".jar")) {

                File srcFile = new File(loc.getPath());

                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                int ans = chooser.showSaveDialog(null);
                if (ans == JFileChooser.APPROVE_OPTION) {

                    File outDir = chooser.getSelectedFile();
                    outDir.mkdirs();

                    ZipFile jarFile = new ZipFile(srcFile);
                    for (Enumeration<? extends ZipEntry> en = jarFile.entries(); en.hasMoreElements();) {
                        ZipEntry e = en.nextElement();
                        InputStream is = jarFile.getInputStream(e);
                        File outFile = new File(outDir, e.getName());
                        if (e.isDirectory()) {
                            outFile.mkdirs();
                        }
                        else {
                            OutputStream os = new FileOutputStream(outFile);

                            byte[] buf = new byte[2048];
                            while (is.available() > 0) {
                                int read = is.read(buf);
                                os.write(buf, 0, read);
                            }
                            is.close();
                            os.close();
                        }
                    }
                    JOptionPane.showMessageDialog(this.frame, "Copied LmcpGen files to " + outDir.getPath());
                    return;
                }
            }

        } catch (Exception ex) {
        }
        JOptionPane.showMessageDialog(this.frame, "Error getting source.  Is this program run from a JAR file?");
    }

    class PackageInfo {

        private final String displayName;
        private final String templateListing;
        private final String methodsClass;
        private final JCheckBox checkBox;
        private final String outputDir;

        public PackageInfo(String displayName, String templateListing, String methodsClass, String outputDir) {
            this.displayName = displayName;
            this.templateListing = templateListing;
            this.methodsClass = methodsClass;
            this.checkBox = new JCheckBox(displayName);
            this.outputDir = outputDir;
        }
    }

    class MultiFilePanel extends JPanel {

        JFileChooser chooser = new JFileChooser();
        JButton addbut = new JButton();
        JButton removebut = new JButton();
        ArrayList<String> files = new ArrayList<String>();
        JList fileList = new JList();
        JPanel butPanel = new JPanel();

        public MultiFilePanel(String addButtonText, String removeButtonText) {
            butPanel.setLayout(new BoxLayout(butPanel, BoxLayout.Y_AXIS));
            addbut.setText(addButtonText);
            chooser.setMultiSelectionEnabled(true);

            addbut.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    chooser.setCurrentDirectory(lastDirectory);
                    int res = chooser.showDialog(MultiFilePanel.this, "Select");
                    if (res == JFileChooser.APPROVE_OPTION) {
                        for (File f : chooser.getSelectedFiles()) {
                            files.add(f.getAbsolutePath());
                            fileList.setListData(files.toArray());
                        }
                        lastDirectory = chooser.getSelectedFile().getParentFile();
                    }
                }
            });

            removebut.setText(removeButtonText);
            removebut.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    Object[] sels = fileList.getSelectedValues();
                    files.removeAll(Arrays.asList(sels));
                    fileList.setListData(files.toArray());
                }
            });

            JScrollPane sp = new JScrollPane(fileList);
            sp.setPreferredSize(new Dimension(300, 100));

            add(sp);
            add(butPanel);
            butPanel.add(addbut);
            butPanel.add(removebut);
        }

        public JFileChooser getChooser() {
            return chooser;
        }

        public String[] getPaths() {
            return files.toArray(new String[]{});
        }

        @Override
        public void setEnabled(boolean enabled) {
            super.setEnabled(enabled);
            for (Component c : getComponents()) {
                c.setEnabled(enabled);
            }
        }
    }
}
