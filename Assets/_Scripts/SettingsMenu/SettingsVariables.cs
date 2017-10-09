using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public static class SettingsVariables {
    public static bool haptics = true;
    public static bool narration = true;
    public static float volume = .50f;
    public static Vector3 volumeBallLocalPos = new Vector3(1, 0, 0);

    public static bool getState(string feature)
    {
        switch (feature)
        {
            case "haptics":
                return haptics;
            case "narration":
                return narration;
            default:
                return true;
        }
    }

    public static void toggleFeature(string feature, bool newState)
    {
        switch (feature)
        {
            case "haptics":
                haptics = newState;
                break;
            case "narration":
                narration = newState;
                break;
        }
    }
}
