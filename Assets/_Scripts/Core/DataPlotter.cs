
#if UNITY_EDITOR

using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;

using Nanome.Core;

public class DataPlotter : EditorWindow
{
    // [BEGIN MENU ITEMS]

    // Open window button
    [MenuItem("Nanome Tools/Data Plotter")]
    static void OpenWindow()
    {
        var window = GetWindow(typeof(DataPlotter));
        window.titleContent = new GUIContent("Data Plotter");
    }

    // [END MENU ITEMS]

    // [BEGIN INTERALS]

    // Channel data
    Channel selectedChannel;
    int channelSelection = 0;
    static List<string> channelNames = new List<string>();
    static Dictionary<string, Channel> channels = new Dictionary<string, Channel>();

    // Textures
    static Texture2D darkGrayTex;
    static Texture2D grayTex;

    // Styles
    GUIStyle darkGrayBoxStyle;
    GUIStyle grayBoxStyle;
    GUIStyle popupStyle;
    GUIStyle toolBarButtonStyle;
    //GUIStyle boxStyle;
    GUIStyle boldLabelStyle;

    // Contents
    GUIContent channelsContent = new GUIContent("");
    GUIContent clearContent = new GUIContent("Clear");

    // Rects
    Rect toolBarRect;
    Rect channelOptionsRect;
    Rect positionLabelRect;
    Rect graphRect;

    // Window data
    Vector2 mousePosition;
    float marginHeight = 5f;
    float marginWidth = 5f;

    // Graph data
    float minXValue;
    float maxXValue;
    float minYValue;
    float maxYValue;
    float xScaleRatio;
    float yScaleRatio;

    // Refresh rate data
    float prevTime;
    float refreshTime;

    // Instance
    static DataPlotter instance;

    // [END INTERALS]

    // [BEGIN GUI]

    // On Enable
    void OnEnable()
    {
        instance = this;

        refreshTime = 0.1f;
        prevTime = Time.time;

        GetTextures();
    }

    // Update
    void Update()
    {
        if (Time.time - prevTime > refreshTime)
        {
            prevTime = Time.time;
            if (instance != null)
            {
                instance.Repaint();
            }
        }
    }

    // Main GUI function
    void OnGUI()
    {
        // Pre draw setup
        PreDraw();
        // Do main draw
        MainDraw();
        // Post draw
        PostDraw();
    }

    void GetTextures()
    {
        grayTex = Resources.Load<Texture2D>("Gray");
        darkGrayTex = Resources.Load<Texture2D>("DarkGray");
    }

    void PreDraw()
    {
        // Make styles
        MakeStyles();

        // Get Inputs
        mousePosition = Event.current.mousePosition;

        // Get current channel
        GetCurrentChannel();
    }

    void MainDraw()
    {
        // Draw tool bar
        DrawToolBar();

        // Draw panes horizontally
        EditorGUILayout.BeginHorizontal(GUILayout.ExpandWidth(true));
        {
            // Draw channel options on the left
            DrawChannelOptions();

            // Draw graph on the right
            DrawGraph();
        }
        EditorGUILayout.EndHorizontal();
    }

    void PostDraw()
    {
        // Repaint if GUI changed
        if (GUI.changed) { Repaint(); }
    }

    void MakeStyles()
    {
        // Make new styles
        darkGrayBoxStyle = new GUIStyle();
        darkGrayBoxStyle.normal.background = darkGrayTex;
        grayBoxStyle = new GUIStyle();
        grayBoxStyle.normal.background = grayTex;

        // Get built in GUI styles
        popupStyle = GUI.skin.GetStyle("ToolbarPopup");
        toolBarButtonStyle = EditorStyles.toolbarButton;
        //boxStyle = GUI.skin.box;
        boldLabelStyle = EditorStyles.boldLabel;
    }

    void GetCurrentChannel()
    {
        // Clamp selection index
        channelSelection = Mathf.Clamp(channelSelection, 0, channelNames.Count - 1);

        // Get selection
        if (channelNames.Count != 0)
        {
            var channelName = channelNames[channelSelection];
            channels.TryGetValue(channelName, out selectedChannel);
        }
    }

    void ClearAll()
    {
        foreach (var channel in channels.Values)
        {
            channel.Clear();
        }
    }

    void DrawToolBar()
    {
        // Draw horizontal tool bar
        var tempToolBarRect = EditorGUILayout.BeginHorizontal(EditorStyles.toolbar, GUILayout.Height(EditorStyles.toolbar.fixedHeight), GUILayout.ExpandWidth(true));
        {
            if ((Event.current.type != EventType.Layout) && (Event.current.type != EventType.Used))
            {
                // Get tool bar rect
                toolBarRect = tempToolBarRect;
            }

            // Clear button
            if (GUILayout.Button(clearContent, toolBarButtonStyle, GUILayout.Width(toolBarButtonStyle.CalcSize(clearContent).x)))
            {
                ClearAll();
            }

            // Flexible space
            GUILayout.FlexibleSpace();
        }
        EditorGUILayout.EndHorizontal();
    }

    void DrawChannelOptions()
    {
        // Draw options vertically
        var tempChannelOptionsRect = EditorGUILayout.BeginVertical(GUILayout.Width(200));
        {
            if ((Event.current.type != EventType.Layout) && (Event.current.type != EventType.Used))
            {
                // Get Channel options rect rect
                channelOptionsRect = tempChannelOptionsRect;
            }

            if (selectedChannel != null)
            {
                // Get current channel name
                channelsContent.text = channelNames[channelSelection];

                // Channel label
                GUIContent channelLabelContent = new GUIContent("Channel:");
                EditorGUILayout.LabelField(channelLabelContent, boldLabelStyle);

                // Draw channel options as popup
                channelSelection = EditorGUILayout.Popup(channelSelection, channelNames.ToArray(), popupStyle, GUILayout.Width(popupStyle.CalcSize(channelsContent).x));

                // Data sets label
                GUIContent dataSetsLabelContent = new GUIContent("Data Sets:");
                EditorGUILayout.LabelField(dataSetsLabelContent, boldLabelStyle);
                // Draw dataset buttons for selected channel
                foreach (var dataSet in selectedChannel.Values())
                {
                    DrawDataSetButton(dataSet);
                }
            }
            // Fill in empty space with gray background
            GUILayout.Box("", grayBoxStyle, GUILayout.ExpandHeight(true), GUILayout.ExpandWidth(true));
        }
        EditorGUILayout.EndVertical();
    }

    void DrawGraph()
    {
        // Draw graph vertically
        EditorGUILayout.BeginVertical();
        {
            // Draw position label above graph
            var tempPositionLabelRect = EditorGUILayout.BeginHorizontal();
            {
                var xPosScreen = mousePosition.x - channelOptionsRect.width;
                var yPosScreen = mousePosition.y - toolBarRect.height - positionLabelRect.height;
                GUILayout.TextArea("(" + ToGraphSpace(xPosScreen, minXValue, xScaleRatio) + ", " + ToGraphSpace(yPosScreen, maxYValue, -yScaleRatio) + ")");
            }
            EditorGUILayout.EndHorizontal();

            // Draw Graph
            var tempGraphRect = EditorGUILayout.BeginVertical();
            {
                if ((Event.current.type != EventType.Layout) && (Event.current.type != EventType.Used))
                {
                    // Get position label rect and graph rect
                    positionLabelRect = tempPositionLabelRect;
                    positionLabelRect.height += 4f; // Warning for some reason the rect isn't exactly correct, need to add 2 pixels
                    graphRect = tempGraphRect;
                }

                // Draw background
                GUILayout.Box("", darkGrayBoxStyle, GUILayout.ExpandHeight(true), GUILayout.ExpandWidth(true));
                // Draw graph data on top of background in screen space
                GUILayout.BeginArea(new Rect(channelOptionsRect.width + marginWidth, toolBarRect.height + positionLabelRect.height + marginHeight, graphRect.width - marginWidth - marginWidth, graphRect.height - marginHeight - marginHeight));
                {
                    // Begin drawing lines
                    Handles.BeginGUI();
                    {
                        // Get Data Set's Max and Mins
                        GetDataSetMaxMin();

                        // Draw Legend Lines
                        DrawLegendLines();

                        // Draw points and connecting lines
                        DrawDataSets();
                    }
                    Handles.EndGUI();
                }
                GUILayout.EndArea();
            }
            EditorGUILayout.EndVertical();
        }
        EditorGUILayout.EndVertical();
    }

    void DrawDataSets()
    {
        // If channel is selected
        if (selectedChannel != null)
        {
            // Draw all toggled datasets in the channel
            foreach (var dataSet in selectedChannel.Values())
            {
                if (dataSet.toggled)
                {
                    DrawDataSet(dataSet);
                }
            }
        }
    }

    void DrawDataSetButton(DataSet dataSet)
    {
        EditorGUILayout.BeginHorizontal();
        {
            dataSet.toggled = GUILayout.Toggle(dataSet.toggled, dataSet.name);
            dataSet.color = EditorGUILayout.ColorField(dataSet.color);
        }
        EditorGUILayout.EndHorizontal();
    }

    void DrawLegendLines()
    {
        // Get min and max "tick marks" for legend lines
        var minXTick = ToScreenSpace(minXValue, minXValue, xScaleRatio);
        var maxXTick = ToScreenSpace(maxXValue, minXValue, xScaleRatio);
        var zeroXTick = ToScreenSpace(0, minXValue, xScaleRatio);
        var minYTick = ToScreenSpace(maxYValue, maxYValue, -yScaleRatio);
        var maxYTick = ToScreenSpace(minYValue, maxYValue, -yScaleRatio);
        var zeroYTick = ToScreenSpace(0, maxYValue, -yScaleRatio);

        // Draw legend lines
        Handles.color = new Color(0.7f, 0.7f, 0.7f);
        Handles.DrawPolyLine(new Vector2(minXTick, zeroYTick), new Vector2(maxXTick, zeroYTick));
        Handles.DrawPolyLine(new Vector2(zeroXTick, minYTick), new Vector2(zeroXTick, maxYTick));
    }

    void DrawDataSet(DataSet dataSet)
    {
        // Get color from color picker (always Alpha = 1)
        Handles.color = new Color(dataSet.color.r, dataSet.color.g, dataSet.color.b, 1);

        // Get sorted x values
        var xValues = dataSet.SortedKeys();

        // Draw data as lines
        var prevPosition = Vector2.zero;
        for (int i = 0; i < xValues.Count; i++)
        {
            // Get x and y
            var xValue = xValues[i];
            var yValue = dataSet.points[xValue];

            // Convert to position in screen space
            var newPosition = new Vector2(ToScreenSpace(xValue, minXValue, xScaleRatio), ToScreenSpace(yValue, maxYValue, -yScaleRatio));

            // Don't draw a line until you have at least 2 points
            if (i != 0)
            {
                // Draw lines between prev and current point
                Handles.DrawPolyLine(prevPosition, newPosition);
            }
            prevPosition = newPosition;
        }
    }

    void GetDataSetMaxMin()
    {
        // Start min and max values at inf and -inf
        minXValue = Mathf.Infinity;
        maxXValue = Mathf.NegativeInfinity;

        minYValue = Mathf.Infinity;
        maxYValue = Mathf.NegativeInfinity;

        // Only graph data if there is a channel
        if (selectedChannel != null)
        {
            // Get min and max values from toggled datasets
            foreach (var dataSet in selectedChannel.Values())
            {
                if (dataSet.toggled)
                {
                    if (dataSet.MinX() < minXValue)
                    {
                        minXValue = dataSet.MinX();
                    }
                    if (dataSet.MaxX() > maxXValue)
                    {
                        maxXValue = dataSet.MaxX();
                    }
                    if (dataSet.MinY() < minYValue)
                    {
                        minYValue = dataSet.MinY();
                    }
                    if (dataSet.MaxY() > maxYValue)
                    {
                        maxYValue = dataSet.MaxY();
                    }
                }
            }
        }

        // Some buffer space for the margins of the graph
        var bufferSpace = 0.01f;

        // Add or subtract the buffer space
        minXValue -= bufferSpace;
        maxXValue += bufferSpace;
        minYValue -= bufferSpace;
        maxYValue += bufferSpace;

        // Get ratio between data set space and graph space
        xScaleRatio = (graphRect.width - marginWidth - marginWidth) / (maxXValue - minXValue);
        yScaleRatio = (graphRect.height - marginHeight - marginHeight) / (maxYValue - minYValue);
    }

    // Helper function to convert from graph to screen space
    float ToScreenSpace(float value, float shiftValue, float scaleRatio)
    {
        return ((value - shiftValue) * scaleRatio);
    }

    // Helper function to convert from screen to graph space
    float ToGraphSpace(float value, float shiftValue, float scaleRatio)
    {
        return (value / scaleRatio) + shiftValue;
    }

    // [END GUI]

    // [BEGIN API]

    // Add point to a data set on a channel
    public static void AddDataPoint(string channelName, string dataSetName, float x, float y, Color? color = null)
    {
        Channel channel;
        DataSet dataSet;

        // Try to get the channel
        if (!channels.TryGetValue(channelName, out channel))
        {
            // Make channel if not present
            channel = AddChannel(channelName);
        }

        // Try to get the dataset
        if (!channel.dataSetsByName.TryGetValue(dataSetName, out dataSet))
        {
            // Make dataset if not present
            Color myColor;
            if (color == null)
            {
                var hue = Math.Abs(dataSetName.GetHashCode()) % 1000f / 1000f;
                myColor = Color.HSVToRGB(hue, 1f, 1f);
            }
            else
            {
                myColor = (Color)color;
            }
            dataSet = AddDataSetToChannel(channelName, dataSetName, myColor);
        }

        // Add new point to data set
        dataSet.AddPoint(x, y);

    }

    // Helper function to add a channel
    static Channel AddChannel(string channelName)
    {
        Channel channel = new Channel(channelName);
        channelNames.Add(channelName);
        channels[channelName] = channel;
        return channel;
    }

    // Helper function to add a dataset to a channel
    static DataSet AddDataSetToChannel(string channelName, string dataSetName, Color color)
    {
        DataSet dataSet = new DataSet(dataSetName);
        dataSet.color = color;
        channels[channelName].dataSetsByName[dataSetName] = dataSet;
        return dataSet;
    }

    // [END API]
}

internal class DataSet
{
    // Public var
    public string name;
    public Color color;
    public bool toggled = true;
    public Dictionary<float, float> points = new Dictionary<float, float>();

    // Private vars
    float minX, maxX;
    float minY, maxY;
    List<float> sortedValues = new List<float>();
    List<float> sortedKeys = new List<float>();

    // Constructor
    public DataSet(string name)
    {
        this.name = name;
    }

    // Getters
    public float MinX() { return minX; }
    public float MaxX() { return maxX; }
    public float MinY() { return minY; }
    public float MaxY() { return maxY; }
    public List<float> Keys() { return new List<float>(points.Keys); }
    public List<float> Values() { return new List<float>(points.Values); }
    public List<float> SortedValues() { return sortedValues; }
    public List<float> SortedKeys() { return sortedKeys; }

    // Public functions
    public void AddPoint(float x, float y)
    {
        points[x] = y;

        sortedKeys.Add(x);
        sortedKeys.Sort();
        CalcMinMax(sortedKeys, out minX, out maxX);

        sortedValues.Add(y);
        sortedValues.Sort();
        CalcMinMax(sortedValues, out minY, out maxY);
    }

    public void Clear()
    {
        points.Clear();
        sortedKeys.Clear();
        sortedValues.Clear();
    }

    // Internal functions
    void CalcMinMax(List<float> list, out float min, out float max)
    {
        min = list[0];
        max = list[list.Count - 1];
    }
}

internal class Channel
{
    // Public vars
    public string name;
    public Dictionary<string, DataSet> dataSetsByName = new Dictionary<string, DataSet>();

    // Constructor
    public Channel(string name)
    {
        this.name = name;
    }

    // Getters
    public List<string> Keys() { return new List<string>(dataSetsByName.Keys); }
    public List<DataSet> Values() { return new List<DataSet>(dataSetsByName.Values); }

    // Public functions
    public void Clear()
    {
        foreach (var value in Values())
        {
            value.Clear();
        }
    }
}

#else

using UnityEngine;

public class DataPlotter
{
    public static void AddDataPoint(string channelName, string dataSetName, float x, float y, Color? color = null)
    {
        // Do nothing in builds
    }
}

#endif
