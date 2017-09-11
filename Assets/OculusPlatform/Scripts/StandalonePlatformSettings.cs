namespace Oculus.Platform
{
  using UnityEngine;

  // This only exists for the Unity Editor
  public sealed class StandalonePlatformSettings : ScriptableObject
  {
    private const string OculusPlatformAccessTokenKey = "OculusPlatformAccessToken";

    public static string OculusPlatformAccessToken
    {
      get
      {
        var accessToken = "";
#if UNITY_EDITOR
        accessToken = UnityEditor.EditorPrefs.GetString(OculusPlatformAccessTokenKey);
#endif
        return accessToken;
      }

      set
      {
#if UNITY_EDITOR
        UnityEditor.EditorPrefs.SetString(OculusPlatformAccessTokenKey, value);
#endif
      }
    }
  }
}
