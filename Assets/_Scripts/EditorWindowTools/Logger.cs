
#if UNITY_EDITOR

using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;

using Nanome.Core.Extension;
using Nanome.Core;

public class Logger : EditorWindow
{
    // [BEGIN MENU ITEMS]

    // Menu button to open this window
    [MenuItem("Nanome Tools/Logger")]
    static void OpenWindow()
    {
        var window = GetWindow(typeof(Logger));
        window.titleContent = new GUIContent("Logger");
    }

    // [END MENU ITEMS]

    // [BEGIN INTERALS]

    // Log data
    static List<CustomLog> logs = new List<CustomLog>();
    static Dictionary<string, int> collapsedLogs = new Dictionary<string, int>();
    static List<CustomLog> displayedLogs = new List<CustomLog>();
    CustomLog selectedLog;

    // Window data
    bool resizing = false;
    float ratio = 0.7f;
    const int MinBoxSize = 28;
    bool collapseToggled = false;
    //bool clearOnPlay = false;
    //bool errorPause = false;
    int channelSelection = 0;
    string searchString = "";
    string searchStringLower = "";
    bool debugToggled = true;
    bool warningToggled = true;
    bool errorToggled = true;
    int numDebugs = 0;
    int numWarnings = 0;
    int numErrors = 0;
    Vector2 logMessageScrollValues = Vector2.zero;
    Vector2 logDetailsScrollValues = Vector2.zero;
    static List<string> channels = new List<string>
    {
        "Channel: All",
        "Channel: Default"
    };
    bool followScroll;
    float prevMaxScrollHeight;
    int prevDisplayedLogCount;

    // Textures
    Texture2D debugIconSmall;
    Texture2D warningIconSmall;
    Texture2D errorIconSmall;
    Texture2D debugIcon;
    Texture2D boxBgEvenLight;
    Texture2D boxBgOddLight;
    Texture2D boxBgSelectedLight;

    // Styles
    GUIStyle boxStyle;
    GUIStyle logBoxStyle;
    GUIStyle detailsBoxStyle;
    GUIStyle verticalScrollbarStyle;
    GUIStyle toolBarButtonStyle;
    GUIStyle popupStyle;
    GUIStyle toolBarSearchTextFieldStyle;
    GUIStyle toolbarSeachCancelButtonStyle;
    // Styles
    GUIStyle textStyle;
    GUIStyle iconStyle;
    GUIStyle badgeCountStyle;

    // Contents
    GUIContent clearContent = new GUIContent("Clear");
    GUIContent collapseContent = new GUIContent("Collapse");
    GUIContent channelsContent = new GUIContent("");
    GUIContent searchStringContent = new GUIContent("");
    GUIContent debugContent = new GUIContent("Debug");
    GUIContent warningContent = new GUIContent("Warning");
    GUIContent errorContent = new GUIContent("Error");
    GUIContent textContent = new GUIContent();
    GUIContent iconContent = new GUIContent();
    GUIContent collapsedBadgeContent = new GUIContent();

    // Rects
    Rect rect;
    Rect resizeRect = new Rect(0, 0, 0, 10);

    // [END INTERALS]

    // [BEGIN GUI]

    void OnEnable()
    {
        // Set initial tracking vars
        prevDisplayedLogCount = 0;
        prevMaxScrollHeight = 0.0f;
        followScroll = true;
        // Get textures
        GetTextures();
    }

    void Update()
    {
        // Force new frame
        Repaint();
    }

    void OnGUI()
    {
        try
        {
            // Pre draw setup
            PreDraw();
            // Do main draw
            MainDraw();
        }
        catch
        {
            // If there was an error printing the GUI, cancel
            throw new ExitGUIException();
        }
    }

    void PreDraw()
    {
        // Make Styles
        MakeStyles();
        // Get Inputs
        GetInputs();
    }

    void MainDraw()
    {
        // Draw tool bar
        DrawToolBar();
        // Draw messages box and details box
        DrawLogsBoxes();
    }

    void MakeStyles()
    {
        // Make new styles
        logBoxStyle = new GUIStyle();
        detailsBoxStyle = new GUIStyle();
        detailsBoxStyle.wordWrap = true;
        detailsBoxStyle.margin = new RectOffset(2, 2, 2, 2);
        textStyle = new GUIStyle();
        textStyle.alignment = TextAnchor.MiddleLeft;
        iconStyle = new GUIStyle();
        iconStyle.alignment = TextAnchor.MiddleCenter;
        badgeCountStyle = new GUIStyle();
        badgeCountStyle.alignment = TextAnchor.MiddleCenter;

        // Get built in GUI styles
        boxStyle = GUI.skin.GetStyle("CN Box");
        verticalScrollbarStyle = GUI.skin.verticalScrollbar;
        toolBarButtonStyle = EditorStyles.toolbarButton;
        popupStyle = GUI.skin.GetStyle("ToolbarPopup");
        toolBarSearchTextFieldStyle = GUI.skin.GetStyle("ToolbarSeachTextField");
        toolbarSeachCancelButtonStyle = GUI.skin.GetStyle("ToolbarSeachCancelButton");
    }

    void GetTextures()
    {
        errorIconSmall = EditorGUIUtility.Load("icons/console.erroricon.sml.png") as Texture2D;
        warningIconSmall = EditorGUIUtility.Load("icons/console.warnicon.sml.png") as Texture2D;
        debugIconSmall = EditorGUIUtility.Load("icons/console.infoicon.sml.png") as Texture2D;

        debugIcon = EditorGUIUtility.Load("icons/console.infoicon.png") as Texture2D;

        // Get light theme textures
        boxBgOddLight = EditorGUIUtility.Load("builtin skins/lightskin/images/cn entrybackodd.png") as Texture2D;
        boxBgEvenLight = EditorGUIUtility.Load("builtin skins/lightskin/images/cnentrybackeven.png") as Texture2D;
        boxBgSelectedLight = EditorGUIUtility.Load("builtin skins/lightskin/images/menuitemhover.png") as Texture2D;

        if (UnityEditor.EditorGUIUtility.isProSkin)
        {
            boxBgOddLight = EditorGUIUtility.Load("builtin skins/darkskin/images/cn entrybackodd.png") as Texture2D;
            boxBgEvenLight = EditorGUIUtility.Load("builtin skins/darkskin/images/cnentrybackeven.png") as Texture2D;
            boxBgSelectedLight = EditorGUIUtility.Load("builtin skins/darkskin/images/menuitemhover.png") as Texture2D;
        }
        // (NOTE: To get dark theme textures, use "builtin skins/darkskin/images/" + the above png names)
    }

    void DrawToolBar()
    {

        var max = 99999;

        // Get and format num debugs text
        var numDebugsText = Mathf.Clamp(numDebugs, 0, max).ToString();
        var numWarningsText = Mathf.Clamp(numWarnings, 0, max).ToString();
        var numErrorsText = Mathf.Clamp(numErrors, 0, max).ToString();

        if (numDebugs > max)
        {
            numDebugsText += "+";
        }
        if (numWarnings > max)
        {
            numWarningsText += "+";
        }
        if (numErrors > max)
        {
            numErrorsText += "+";
        }

        // Set log severity icons and text
        debugContent.image = debugIconSmall;
        debugContent.text = numDebugsText;
        warningContent.image = warningIconSmall;
        warningContent.text = numWarningsText;
        errorContent.image = errorIconSmall;
        errorContent.text = numErrorsText;

        // Draw horizontal tool bar
        EditorGUILayout.BeginHorizontal(EditorStyles.toolbar, GUILayout.Height(EditorStyles.toolbar.fixedHeight), GUILayout.ExpandWidth(true));
        {
            // Clear button
            if (GUILayout.Button(clearContent, toolBarButtonStyle, GUILayout.Width(toolBarButtonStyle.CalcSize(clearContent).x)))
            {
                logs.Clear();
            }

            // Some fixed space to look pretty
            GUILayout.Space(6f);

            // Collapse Toggle
            collapseToggled = GUILayout.Toggle(collapseToggled, collapseContent, toolBarButtonStyle, GUILayout.Width(toolBarButtonStyle.CalcSize(collapseContent).x));

            // Channel selection popup
            channelSelection = Mathf.Clamp(channelSelection, 0, channels.Count - 1);
            channelsContent.text = channels[channelSelection];
            channelSelection = EditorGUILayout.Popup(channelSelection, channels.ToArray(), popupStyle, GUILayout.Width(popupStyle.CalcSize(channelsContent).x));

            // Flexible space
            GUILayout.FlexibleSpace();

            // Search bar
            searchStringContent.text = searchString;
            searchString = GUILayout.TextField(searchString, toolBarSearchTextFieldStyle, GUILayout.Width(100));
            if (GUILayout.Button("", toolbarSeachCancelButtonStyle))
            {
                // Remove focus if cleared
                searchString = "";
                GUI.FocusControl(null);
            }
            searchStringLower = searchString.ToLower();

            // Log type toggles
            debugToggled = GUILayout.Toggle(debugToggled, debugContent, toolBarButtonStyle, GUILayout.Width(toolBarButtonStyle.CalcSize(debugContent).x));
            warningToggled = GUILayout.Toggle(warningToggled, warningContent, toolBarButtonStyle, GUILayout.Width(toolBarButtonStyle.CalcSize(warningContent).x));
            errorToggled = GUILayout.Toggle(errorToggled, errorContent, toolBarButtonStyle, GUILayout.Width(toolBarButtonStyle.CalcSize(errorContent).x));
        }
        EditorGUILayout.EndHorizontal();
    }

    void DrawLogsBoxes()
    {
        // Vertical group for both boxes
        var tempRect = EditorGUILayout.BeginVertical(GUILayout.ExpandHeight(true));
        {
            // Logic for resizing the boxes correctly
            if ((Event.current.type != EventType.Layout) && (Event.current.type != EventType.Used))
            {
                rect = tempRect;
                resizeRect.width = rect.width;
                resizeRect.y = Mathf.Clamp(rect.height * ratio, MinBoxSize, rect.height - MinBoxSize);
                ratio = (resizeRect.y) / rect.height;
            }

            // Messages Box
            DrawLogBox();

            // Details Box
            DrawDetailsBox();
        }
        EditorGUILayout.EndVertical();

        // Add cursor on resize rect hover
        EditorGUIUtility.AddCursorRect(resizeRect, MouseCursor.ResizeVertical);
    }

    void GetInputs()
    {
        // Get resize events
        if (Event.current.type == EventType.MouseDown && resizeRect.Contains(Event.current.mousePosition))
        {
            resizing = true;
        }
        else if (resizing && Event.current.type == EventType.MouseUp)
        {
            resizing = false;
        }

        if (resizing && (Event.current.type == EventType.MouseDrag))
        {
            resizeRect.y = Event.current.mousePosition.y - rect.y + 10;
            ratio = resizeRect.y / rect.height;
        }

        // Keyboard events

        if (Event.current.type == EventType.KeyDown)
        {
            if (Event.current.keyCode == KeyCode.UpArrow)
            {
                if (selectedLog != null)
                {
                    var index = displayedLogs.IndexOf(selectedLog);
                    if (index > 0)
                    {
                        selectedLog.isSelected = false;
                        selectedLog = displayedLogs[index - 1];
                        selectedLog.isSelected = true;
                    }
                }
            }
            else if (Event.current.keyCode == KeyCode.DownArrow)
            {
                if (selectedLog != null)
                {
                    var index = displayedLogs.IndexOf(selectedLog);
                    if (index < displayedLogs.Count - 1)
                    {
                        selectedLog.isSelected = false;
                        selectedLog = displayedLogs[index + 1];
                        selectedLog.isSelected = true;
                    }
                }
            }
        }
    }

    void DrawLogBox()
    {
        // Clear out temp log list / dictionaries before redrawing
        collapsedLogs.Clear();
        displayedLogs.Clear();

        // Reset num values
        numDebugs = 0;
        numWarnings = 0;
        numErrors = 0;

        // Get collapsed counts
        GetCollapsedCounts();

        // Run log occlusion
        OccludeLogs();

        // Draw Log box vertically
        EditorGUILayout.BeginVertical(GUILayout.Height(rect.height * ratio));
        {
            if (followScroll)
            {
                logMessageScrollValues = new Vector2(logMessageScrollValues.x, prevMaxScrollHeight);
                followScroll = false;
            }
            // Draw vertical scroll view
            logMessageScrollValues = GUILayout.BeginScrollView(logMessageScrollValues, false, false, GUIStyle.none, verticalScrollbarStyle, boxStyle);
            {
                // Draw each log message
                DrawLogs(logMessageScrollValues.y, rect.height * ratio);
            }
            GUILayout.EndScrollView();
        }
        EditorGUILayout.EndVertical();
    }

    void GetCollapsedCounts()
    {
        if (collapseToggled)
        {
            var logsCount = logs.Count;
            for (var i = 0; i < logsCount; i++)
            {
                var log = logs[i];
                int collapsedLogCount = 0;
                collapsedLogs.TryGetValue(log.Message(), out collapsedLogCount);
                collapsedLogCount++;
                collapsedLogs[log.Message()] = collapsedLogCount;
            }
        }
    }

    void OccludeLogs()
    {
        var logsCount = logs.Count;
        // Loop over each logs
        for (var i = 0; i < logsCount; i++)
        {
            var log = logs[i];
            // Check if log type is toggled
            var typeToggled = CheckLogTypeToggled((LogType)log.Severity());
            // Check if channel toggled
            var channelToggled = CheckIfChannelTogged(log.Channel());
            // Check if message is in search results
            var inSearchResults = CheckIfInSearchResults(log.Lower());
            // Check if we should display or collapse the message
            int collapsedLogCount;
            var notCollapsed = TryGetCollapseCount(log.Message(), out collapsedLogCount);
            // If message should be displayed
            var displayed = typeToggled && channelToggled && inSearchResults && notCollapsed;
            if (displayed)
            {
                // Add this log to our displayed logs
                displayedLogs.Add(log);
            }
        }
        CheckIfDisplayedLogCountChanged();
    }

    void DrawLogs(float clipStartY, float clipSizeY)
    {
        // Actual rendering of displayed logs
        var displayedLogsCount = displayedLogs.Count;
        var minLogIdxInView = (int)Math.Max(0, clipStartY / messageHeight - 2);
        var maxLogIdxInView = (int)Math.Min(displayedLogsCount, (clipStartY + clipSizeY) / messageHeight + 2);
        GUILayout.Space(minLogIdxInView * messageHeight);
        // Loop over each logs
        for (var i = 0; i < displayedLogsCount; i++)
        {
            // If log is in view
            var log = displayedLogs[i];
            // If is within range
            if (i >= minLogIdxInView && i < maxLogIdxInView)
            {
                // Render it
                var text = log.Message().Split("\n")[0];
                // Check if we should display or collapse the message
                int collapsedLogCount;
                TryGetCollapseCount(log.Message(), out collapsedLogCount);
                // Draw the message and check if pressed
                if (DrawLog(text, (LogType)(log.Severity()), (i % 2) == 0, log.isSelected, collapsedLogCount))
                {
                    GUI.FocusControl(null);

                    if (selectedLog != null)
                    {
                        // Deselect previous selectedLog
                        selectedLog.isSelected = false;
                    }

                    // This message is now the selectedLog
                    selectedLog = log;
                    // Select this message
                    selectedLog.isSelected = true;

                    // Should not follow scroll anymore
                    followScroll = false;
                    prevMaxScrollHeight = Mathf.Infinity; // cheap trick to not automatically follow scroll when next log is added
                }
            }
        }
        GUILayout.Space((displayedLogsCount - maxLogIdxInView) * messageHeight);
    }

    void DrawDetailsBox()
    {
        EditorGUILayout.BeginVertical();
        {
            // Create vertical scroll view
            logDetailsScrollValues = EditorGUILayout.BeginScrollView(logDetailsScrollValues, false, false, GUIStyle.none, verticalScrollbarStyle, boxStyle);
            {
                if (selectedLog != null)
                {
                    var logMessge = selectedLog.Message();
                    var stackTrace = selectedLog.StackTrace();
                    var details = String.Format("{0}\n\n{1}", logMessge, stackTrace);

                    if (UnityEditor.EditorGUIUtility.isProSkin)
                    {
                        details = "<color=white>" + details + "</color>";
                    }
                    EditorGUILayout.SelectableLabel(details, detailsBoxStyle, GUILayout.ExpandHeight(true));
                }
            }
            EditorGUILayout.EndScrollView();
        }
        EditorGUILayout.EndVertical();
    }

    public enum LogType { Debug, Warning, Exception, Error };

    bool CheckLogTypeToggled(LogType logType)
    {
        // Determine type
        // Increment type
        // Return true if that type if toggled
        var toggled = false;
        switch (logType)
        {
            case LogType.Debug:
                {
                    numDebugs++;
                    if (debugToggled)
                    {
                        toggled = true;
                    }
                }
                break;
            case LogType.Warning:
                {
                    numWarnings++;
                    if (warningToggled)
                    {
                        toggled = true;
                    }
                }
                break;
            case LogType.Exception:
                {
                    numErrors++;
                    if (errorToggled)
                    {
                        toggled = true;
                    }
                }
                break;
            case LogType.Error:
                {
                    numErrors++;
                    if (errorToggled)
                    {
                        toggled = true;
                    }
                }
                break;
        }
        return toggled;
    }

    bool CheckIfChannelTogged(string channel)
    {
        // If channel toggled or all channels toggled, return true
        var toggled = false;
        var myChannelToggled = channels[channelSelection] == channel;
        var allChannelsToggled = channels[channelSelection] == "Channel: All";
        if (myChannelToggled || allChannelsToggled)
        {
            toggled = true;
        }
        return toggled;
    }

    bool CheckIfInSearchResults(string logMessage)
    {
        // If search is empty, display the message
        bool inSearchResults = true;
        if (searchStringLower != "")
        {
            // If message does not contain the substring, searchString, don't display it
            if (!logMessage.Contains(searchStringLower))
            {
                inSearchResults = false;
            }
        }
        return inSearchResults;
    }

    bool TryGetCollapseCount(string logMessage, out int collapsedLogCount)
    {
        collapsedLogCount = 0;
        bool firstOccurance = true;
        if (collapseToggled)
        {
            // Get the collapsed count
            if (collapsedLogs.TryGetValue(logMessage, out collapsedLogCount))
            {
                // Remove from the dictionary after we get the data
                collapsedLogs.Remove(logMessage);
                firstOccurance = true;
            }
            else
            {
                // If the key has been removed, then that means we already printed the log
                firstOccurance = false;
            }
        }
        return firstOccurance;
    }

    float messageHeight = 24.0f;

    bool DrawLog(string text, LogType logType, bool isOdd, bool isSelected, int collapsedLogCount)
    {
        // Current textures
        Texture2D boxBgSelected;
        Texture2D boxBgOdd;
        Texture2D boxBgEven;
        Texture2D icon = debugIcon;

        // Set textures
        // (NOTE: may be based on theme in future)
        boxBgSelected = boxBgSelectedLight;
        boxBgOdd = boxBgOddLight;
        boxBgEven = boxBgEvenLight;

        // Show selected / deselected background
        if (isSelected)
        {
            logBoxStyle.normal.background = boxBgSelected;
        }
        else
        {
            // Show odd color / even color background
            if (isOdd)
            {
                logBoxStyle.normal.background = boxBgOdd;
            }
            else
            {
                logBoxStyle.normal.background = boxBgEven;
            }
        }

        // Get log type icon
        switch (logType)
        {
            case LogType.Debug: icon = debugIconSmall; break;
            case LogType.Warning: icon = warningIconSmall; break;
            case LogType.Exception: icon = errorIconSmall; break;
            case LogType.Error: icon = errorIconSmall; break;
        }

        // Get Collapsed count
        var collapsedLogCountText = Mathf.Clamp(collapsedLogCount, 0, 999).ToString();
        if (collapsedLogCount > 999)
        {
            collapsedLogCountText += "+";
        }

        // Button press bools
        var badgePressed = false;
        var iconPressed = false;
        var textPressed = false;

        // Set Contents
        collapsedBadgeContent.text = "(" + collapsedLogCountText + ")";
        iconContent.image = icon;

        // Set text
        var textDisplayed = text;
        if (textDisplayed != null && textDisplayed.Length > 512)
        {
            textDisplayed = textDisplayed.Substring(0, 512) + "...";
        }
        if (UnityEditor.EditorGUIUtility.isProSkin)
        {
            textDisplayed = "<color=white>" + textDisplayed + "</color>";
        }
        textContent.text = textDisplayed;

        // Draw Log Entry
        GUILayout.BeginHorizontal(logBoxStyle, GUILayout.Height(messageHeight));
        {
            if (collapseToggled)
            {
                // Collapsed count badge icon and num
                badgePressed = GUILayout.Button(collapsedBadgeContent, badgeCountStyle, GUILayout.Width(badgeCountStyle.CalcSize(collapsedBadgeContent).x + 8), GUILayout.Height(messageHeight));
            }
            // Log type icon
            iconPressed = GUILayout.Button(iconContent, iconStyle, GUILayout.Width(messageHeight), GUILayout.Height(messageHeight));
            // Message text
            textPressed = GUILayout.Button(textContent, textStyle, GUILayout.Height(messageHeight));
        }
        GUILayout.EndHorizontal();
        // Return true if at least one button was pressed
        return badgePressed || iconPressed || textPressed;
    }

    // Internal formatting function (Taken from PlayDebug.cs)
    static List<Tuple<string, string, string>> Format(string raw)
    {
        var lines = raw.Split("\n");
        // Carries class name, full file path, explicit file name, line number
        var categorizedLines = new List<Tuple<string, string, string>>();
        for (int i = 0; i < lines.Length; i++)
        {
            var className = "";
            var filePath = "";
            var lineNum = "";
            // Trim beginning of stacktrace messages
            if (lines[i].StartsWith("   at"))
            {
                // Trim off '   at'
                var length = lines[i].Length;
                if (length < 0)
                {
                    continue;
                }
                lines[i] = lines[i].Substring(6, length - 6);

                // Isolate class
                var endOfClassNameIndex = lines[i].IndexOf(')');
                var endOfClassNameNoParamIndex = lines[i].IndexOf('(');
                if (endOfClassNameIndex < 0 || endOfClassNameNoParamIndex < 0)
                {
                    continue;
                }
                //className = lines[i].Substring(0, endOfClassNameIndex + 1);
                className = lines[i].Substring(0, endOfClassNameNoParamIndex);

                // Isolate file path
                var lineNumIndex = lines[i].IndexOf(":line");
                if (lineNumIndex < 0)
                {
                    continue;
                }
                filePath = lines[i].Substring(endOfClassNameIndex + 5, lineNumIndex - endOfClassNameIndex - 5);

                // Isolate line #
                lineNum = lines[i].Substring(lineNumIndex + 1, lines[i].Length - lineNumIndex - 1);
                categorizedLines.Add(Tuple.Create(className, lineNum, @filePath));
            }

        }
        return categorizedLines;
    }

    void CheckIfDisplayedLogCountChanged()
    {
        if (displayedLogs.Count != prevDisplayedLogCount)
        {
            // Readjust scroll height
            ReadjustScrollHeight();
            // Save new displayed log count
            prevDisplayedLogCount = displayedLogs.Count;
        }
    }

    void ReadjustScrollHeight()
    {
        // Calc max height
        var totalWindowHeight = messageHeight * displayedLogs.Count;
        var scrollWindowHeight = rect.height * ratio;
        var scrollWindowHeightRatio = 0.0f;
        if (scrollWindowHeight < totalWindowHeight)
        {
            scrollWindowHeightRatio = 1 - scrollWindowHeight / totalWindowHeight;
        }
        var maxScrollHeight = totalWindowHeight * scrollWindowHeightRatio;

        var epsilon = 2f;

        // If at max, keep following
        if (logMessageScrollValues.y >= prevMaxScrollHeight - epsilon)
        {
            followScroll = true;
        }

        prevMaxScrollHeight = maxScrollHeight;
    }

    // [END GUI]

    // [BEGIN API]

    // Add log
    public static void AddLog(string editorLine, int severity, string stackTrace, string channel = "Default")
    {
        var logChannel = "Channel: " + channel;
        if (!channels.Contains(logChannel))
        {
            channels.Add(logChannel);
        }
        logs.Add(new CustomLog(editorLine, severity, stackTrace, logChannel));
    }

    // [END API]
}

public class CustomLog
{
    // Public
    public bool isSelected = false;

    // Private
    string message;
    int severity;
    string stackTrace;
    string channel;
    string lower;

    // Constructor
    public CustomLog(string message, int severity, string stackTrace, string channel)
    {
        this.message = message;
        this.severity = severity;
        this.stackTrace = stackTrace;
        this.channel = channel;
        this.lower = message.ToLower();
    }

    // Getters
    public string Message() { return message; }
    public int Severity() { return severity; }
    public string StackTrace() { return stackTrace; }
    public string Channel() { return channel; }
    public string Lower() { return lower; }
}

#else

public class Logger
{
    public static void AddLog(string editorLine, int severity, string stackTrace, string channel = "Default")
    {
        // Do nothing in builds
    }
}

#endif