// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;

#pragma warning disable 414
namespace Oculus.Platform
{
  public class CAPI
  {
#if UNITY_STANDALONE_WIN || UNITY_EDITOR_WIN
  #if UNITY_64 || UNITY_EDITOR_64
    public const string DLL_NAME = "LibOVRPlatform64_1";
  #else
    public const string DLL_NAME = "LibOVRPlatform32_1";
  #endif
#elif UNITY_EDITOR || UNITY_EDITOR_64
    public const string DLL_NAME = "ovrplatform";
#else
    public const string DLL_NAME = "ovrplatformloader";
#endif

    private static UTF8Encoding nativeStringEncoding = new UTF8Encoding(false);

    [StructLayout(LayoutKind.Sequential)]
    public struct ovrKeyValuePair {
      public ovrKeyValuePair(string key, string value) {
        key_ = key;
        valueType_ = KeyValuePairType.String;
        stringValue_ = value;

        intValue_ = 0;
        doubleValue_ = 0.0;
      }

      public ovrKeyValuePair(string key, int value) {
        key_ = key;
        valueType_ = KeyValuePairType.Int;
        intValue_ = value;

        stringValue_ = null;
        doubleValue_ = 0.0;
      }

      public ovrKeyValuePair(string key, double value) {
        key_ = key;
        valueType_ = KeyValuePairType.Double;
        doubleValue_ = value;

        stringValue_ = null;
        intValue_ = 0;
      }

      public string key_;
      KeyValuePairType valueType_;

      public string stringValue_;
      public int intValue_;
      public double doubleValue_;
    };

    public static IntPtr ArrayOfStructsToIntPtr(Array ar)
    {
      int totalSize = 0;
      for(int i=0; i<ar.Length; i++) {
        totalSize += Marshal.SizeOf(ar.GetValue(i));
      }

      IntPtr childrenPtr = Marshal.AllocHGlobal(totalSize);
      IntPtr curr = childrenPtr;
      for(int i=0; i<ar.Length; i++) {
        Marshal.StructureToPtr(ar.GetValue(i), curr, false);
        curr = (IntPtr)((long)curr + Marshal.SizeOf(ar.GetValue(i)));
      }
      return childrenPtr;
    }

    public static CAPI.ovrKeyValuePair[] DictionaryToOVRKeyValuePairs(Dictionary<string, object> dict)
    {
      if(dict == null || dict.Count == 0)
      {
        return null;
      }

      var nativeCustomData = new CAPI.ovrKeyValuePair[dict.Count];

      int i = 0;
      foreach(var item in dict)
      {
        if(item.Value.GetType() == typeof(int))
        {
          nativeCustomData[i] = new CAPI.ovrKeyValuePair(item.Key, (int)item.Value);
        }
        else if(item.Value.GetType() == typeof(string))
        {
          nativeCustomData[i] = new CAPI.ovrKeyValuePair(item.Key, (string)item.Value);
        }
        else if(item.Value.GetType() == typeof(double))
        {
          nativeCustomData[i] = new CAPI.ovrKeyValuePair(item.Key, (double)item.Value);
        }
        else
        {
          throw new Exception("Only int, double or string are allowed types in CustomQuery.data");
        }
        i++;
      }
      return nativeCustomData;
    }

    public static byte[] IntPtrToByteArray(IntPtr data, ulong size)
    {
      byte[] outArray = new byte[size];
      Marshal.Copy(data, outArray, 0, (int)size);
      return outArray;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct ovrMatchmakingCriterion {
      public ovrMatchmakingCriterion(string key, MatchmakingCriterionImportance importance)
      {
        key_ = key;
        importance_ = importance;

        parameterArray = IntPtr.Zero;
        parameterArrayCount = 0;
      }

      public string key_;
      public MatchmakingCriterionImportance importance_;

      public IntPtr parameterArray;
      public uint parameterArrayCount;
    };

    [StructLayout(LayoutKind.Sequential)]
    public struct ovrMatchmakingCustomQueryData {
      public IntPtr dataArray;
      public uint dataArrayCount;

      public IntPtr criterionArray;
      public uint criterionArrayCount;
    };

    public static Dictionary<string, string> DataStoreFromNative(IntPtr pointer) {
      var d = new Dictionary<string, string>();
      var size = (int)CAPI.ovr_DataStore_GetNumKeys(pointer);
      for (var i = 0; i < size; i++) {
        string key = CAPI.ovr_DataStore_GetKey(pointer, i);
        d[key] = CAPI.ovr_DataStore_GetValue(pointer, key);
      }
      return d;
    }

    public static string StringFromNative(IntPtr pointer) {
      if (pointer == IntPtr.Zero) {
        return null;
      }
      var l = GetNativeStringLengthNotIncludingNullTerminator(pointer);
      var data = new byte[l];
      Marshal.Copy(pointer, data, 0, l);
      return nativeStringEncoding.GetString(data);
    }

    public static int GetNativeStringLengthNotIncludingNullTerminator(IntPtr pointer) {
      var l = 0;
      while (true) {
        if (Marshal.ReadByte(pointer, l) == 0) {
          return l;
        }
        l++;
      }
    }

    public static DateTime DateTimeFromNative(ulong seconds_since_the_one_true_epoch) {
      var dt = new DateTime(1970, 1, 1, 0, 0, 0, 0);
      return dt.AddSeconds(seconds_since_the_one_true_epoch).ToLocalTime();
    }

    public static byte[] BlobFromNative(uint size, IntPtr pointer) {
      var a = new byte[(int)size];
      for (int i = 0; i < (int)size; i++) {
        a[i] = Marshal.ReadByte(pointer, i);
      }
      return a;
    }

    public static byte[] FiledataFromNative(uint size, IntPtr pointer) {
      var data = new byte[(int)size];
      Marshal.Copy(pointer, data, 0, (int)size);
      return data;
    }

    public static IntPtr StringToNative(string s) {
      if (s == null) {
        throw new Exception("StringFromNative: null argument");
      }
      var l = nativeStringEncoding.GetByteCount(s);
      var data = new byte[l + 1];
      nativeStringEncoding.GetBytes(s, 0, s.Length, data, 0);
      var pointer = Marshal.AllocCoTaskMem(l + 1);
      Marshal.Copy(data, 0, pointer, l + 1);
      return pointer;
    }

    // Initialization
    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_UnityInitWrapper(string appId);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_UnityInitWrapperAsynchronous(string appId);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_UnityInitWrapperStandalone(string accessToken, IntPtr loggingCB);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_UnityInitWrapperWindows(string appId, IntPtr loggingCB);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_UnityInitWrapperWindowsAsynchronous(string appId, IntPtr loggingCB);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_SetDeveloperAccessToken(string accessToken);

    public static string ovr_GetLoggedInUserLocale() {
      var result = StringFromNative(ovr_GetLoggedInUserLocale_Native());
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_GetLoggedInUserLocale")]
    private static extern IntPtr ovr_GetLoggedInUserLocale_Native();


    // Message queue access

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_PopMessage();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_FreeMessage(IntPtr message);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_NetworkingPeer_GetSendPolicy(IntPtr networkingPeer);


    // VOIP

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Voip_CreateEncoder();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_DestroyEncoder(IntPtr encoder);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Voip_CreateDecoder();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_DestroyDecoder(IntPtr decoder);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_VoipDecoder_Decode(IntPtr obj, byte[] compressedData, ulong compressedSize);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Microphone_Create();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Microphone_Destroy(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_SetSystemVoipPassthrough(bool passthrough);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_SetSystemVoipMicrophoneMuted(VoipMuteState muted);

    // Misc

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_UnityResetTestPlatform();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_HTTP_GetWithMessageType(string url, int messageType);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_CrashApplication();

    public const int VoipFilterBufferSize = 480;
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    public delegate void FilterCallback([MarshalAs(UnmanagedType.LPArray, SizeConst = VoipFilterBufferSize), In, Out] short[] pcmData, UIntPtr pcmDataLength, int frequency, int numChannels);

    [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
    public static extern void ovr_Voip_SetMicrophoneFilterCallback(FilterCallback cb);

    [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
    public static extern void ovr_Voip_SetMicrophoneFilterCallbackWithFixedSizeBuffer(FilterCallback cb, UIntPtr bufferSizeElements);


    // Logging

    public static void LogNewEvent(string eventName, Dictionary<string, string> values) {
      var eventNameNative = StringToNative(eventName);

      var count = values == null ? 0 : values.Count;

      IntPtr[] valuesNative = new IntPtr[count * 2];

      if (count > 0) {
        int i = 0;
        foreach(var item in values) {
          valuesNative[i * 2 + 0] = StringToNative(item.Key);
          valuesNative[i * 2 + 1] = StringToNative(item.Value);
          i++;
        }
      }

      ovr_Log_NewEvent(eventNameNative, valuesNative, (UIntPtr)count);

      Marshal.FreeCoTaskMem(eventNameNative);
      foreach (var nativeItem in valuesNative) {
        Marshal.FreeCoTaskMem(nativeItem);
      }
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Log_NewEvent(IntPtr eventName, IntPtr[] values, UIntPtr length);


    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_ApplicationLifecycle_GetLaunchDetails();

    public static ulong ovr_HTTP_StartTransfer(string url, ovrKeyValuePair[] headers) {
      IntPtr url_native = StringToNative(url);
      UIntPtr headers_length = (UIntPtr)headers.Length;
      var result = (ovr_HTTP_StartTransfer_Native(url_native, headers, headers_length));
      Marshal.FreeCoTaskMem(url_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_HTTP_StartTransfer")]
    private static extern ulong ovr_HTTP_StartTransfer_Native(IntPtr url, ovrKeyValuePair[] headers, UIntPtr numItems);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_HTTP_Write(ulong transferId, byte[] bytes, UIntPtr length);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_HTTP_WriteEOM(ulong transferId);

    public static string ovr_Message_GetStringForJavascript(IntPtr message) {
      var result = StringFromNative(ovr_Message_GetStringForJavascript_Native(message));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Message_GetStringForJavascript")]
    private static extern IntPtr ovr_Message_GetStringForJavascript_Native(IntPtr message);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Net_Accept(UInt64 peerID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_Net_AcceptForCurrentRoom();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Net_Close(UInt64 peerID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Net_CloseForCurrentRoom();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Net_Connect(UInt64 peerID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_Net_IsConnected(UInt64 peerID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Net_Ping(UInt64 peerID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Net_ReadPacket();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_Net_SendPacket(UInt64 userID, UIntPtr length, byte[] bytes, SendPolicy policy);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_Net_SendPacketToCurrentRoom(UIntPtr length, byte[] bytes, SendPolicy policy);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_Accept(UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Voip_GetOutputBufferMaxSize();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Voip_GetPCM(UInt64 senderID, Int16[] outputBuffer, UIntPtr outputBufferNumElements);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Voip_GetPCMFloat(UInt64 senderID, float[] outputBuffer, UIntPtr outputBufferNumElements);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Voip_GetPCMSize(UInt64 senderID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Voip_GetPCMWithTimestamp(UInt64 senderID, Int16[] outputBuffer, UIntPtr outputBufferNumElements, UInt32[] timestamp);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Voip_GetPCMWithTimestampFloat(UInt64 senderID, float[] outputBuffer, UIntPtr outputBufferNumElements, UInt32[] timestamp);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt32 ovr_Voip_GetSyncTimestamp(UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern long ovr_Voip_GetSyncTimestampDifference(UInt32 lhs, UInt32 rhs);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern VoipMuteState ovr_Voip_GetSystemVoipMicrophoneMuted();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern SystemVoipStatus ovr_Voip_GetSystemVoipStatus();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_SetMicrophoneMuted(VoipMuteState state);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_SetOutputSampleRate(VoipSampleRate rate);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_Start(UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Voip_Stop(UInt64 userID);

    public static ulong ovr_Achievements_AddCount(string name, ulong count) {
      IntPtr name_native = StringToNative(name);
      var result = (ovr_Achievements_AddCount_Native(name_native, count));
      Marshal.FreeCoTaskMem(name_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Achievements_AddCount")]
    private static extern ulong ovr_Achievements_AddCount_Native(IntPtr name, ulong count);

    public static ulong ovr_Achievements_AddFields(string name, string fields) {
      IntPtr name_native = StringToNative(name);
      IntPtr fields_native = StringToNative(fields);
      var result = (ovr_Achievements_AddFields_Native(name_native, fields_native));
      Marshal.FreeCoTaskMem(name_native);
      Marshal.FreeCoTaskMem(fields_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Achievements_AddFields")]
    private static extern ulong ovr_Achievements_AddFields_Native(IntPtr name, IntPtr fields);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Achievements_GetAllDefinitions();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Achievements_GetAllProgress();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Achievements_GetDefinitionsByName(string[] names, int count);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Achievements_GetProgressByName(string[] names, int count);

    public static ulong ovr_Achievements_Unlock(string name) {
      IntPtr name_native = StringToNative(name);
      var result = (ovr_Achievements_Unlock_Native(name_native));
      Marshal.FreeCoTaskMem(name_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Achievements_Unlock")]
    private static extern ulong ovr_Achievements_Unlock_Native(IntPtr name);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Application_ExecuteCoordinatedLaunch(ulong appID, ulong roomID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Application_GetInstalledApplications();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Application_GetVersion();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_ApplicationLifecycle_GetRegisteredPIDs();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_ApplicationLifecycle_GetSessionKey();

    public static ulong ovr_ApplicationLifecycle_RegisterSessionKey(string sessionKey) {
      IntPtr sessionKey_native = StringToNative(sessionKey);
      var result = (ovr_ApplicationLifecycle_RegisterSessionKey_Native(sessionKey_native));
      Marshal.FreeCoTaskMem(sessionKey_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_ApplicationLifecycle_RegisterSessionKey")]
    private static extern ulong ovr_ApplicationLifecycle_RegisterSessionKey_Native(IntPtr sessionKey);

    public static ulong ovr_Avatar_UpdateMetaData(string avatarMetaData, string imageFilePath) {
      IntPtr avatarMetaData_native = StringToNative(avatarMetaData);
      IntPtr imageFilePath_native = StringToNative(imageFilePath);
      var result = (ovr_Avatar_UpdateMetaData_Native(avatarMetaData_native, imageFilePath_native));
      Marshal.FreeCoTaskMem(avatarMetaData_native);
      Marshal.FreeCoTaskMem(imageFilePath_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Avatar_UpdateMetaData")]
    private static extern ulong ovr_Avatar_UpdateMetaData_Native(IntPtr avatarMetaData, IntPtr imageFilePath);

    public static ulong ovr_CloudStorage_Delete(string bucket, string key) {
      IntPtr bucket_native = StringToNative(bucket);
      IntPtr key_native = StringToNative(key);
      var result = (ovr_CloudStorage_Delete_Native(bucket_native, key_native));
      Marshal.FreeCoTaskMem(bucket_native);
      Marshal.FreeCoTaskMem(key_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_Delete")]
    private static extern ulong ovr_CloudStorage_Delete_Native(IntPtr bucket, IntPtr key);

    public static ulong ovr_CloudStorage_Load(string bucket, string key) {
      IntPtr bucket_native = StringToNative(bucket);
      IntPtr key_native = StringToNative(key);
      var result = (ovr_CloudStorage_Load_Native(bucket_native, key_native));
      Marshal.FreeCoTaskMem(bucket_native);
      Marshal.FreeCoTaskMem(key_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_Load")]
    private static extern ulong ovr_CloudStorage_Load_Native(IntPtr bucket, IntPtr key);

    public static ulong ovr_CloudStorage_LoadBucketMetadata(string bucket) {
      IntPtr bucket_native = StringToNative(bucket);
      var result = (ovr_CloudStorage_LoadBucketMetadata_Native(bucket_native));
      Marshal.FreeCoTaskMem(bucket_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_LoadBucketMetadata")]
    private static extern ulong ovr_CloudStorage_LoadBucketMetadata_Native(IntPtr bucket);

    public static ulong ovr_CloudStorage_LoadConflictMetadata(string bucket, string key) {
      IntPtr bucket_native = StringToNative(bucket);
      IntPtr key_native = StringToNative(key);
      var result = (ovr_CloudStorage_LoadConflictMetadata_Native(bucket_native, key_native));
      Marshal.FreeCoTaskMem(bucket_native);
      Marshal.FreeCoTaskMem(key_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_LoadConflictMetadata")]
    private static extern ulong ovr_CloudStorage_LoadConflictMetadata_Native(IntPtr bucket, IntPtr key);

    public static ulong ovr_CloudStorage_LoadHandle(string handle) {
      IntPtr handle_native = StringToNative(handle);
      var result = (ovr_CloudStorage_LoadHandle_Native(handle_native));
      Marshal.FreeCoTaskMem(handle_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_LoadHandle")]
    private static extern ulong ovr_CloudStorage_LoadHandle_Native(IntPtr handle);

    public static ulong ovr_CloudStorage_LoadMetadata(string bucket, string key) {
      IntPtr bucket_native = StringToNative(bucket);
      IntPtr key_native = StringToNative(key);
      var result = (ovr_CloudStorage_LoadMetadata_Native(bucket_native, key_native));
      Marshal.FreeCoTaskMem(bucket_native);
      Marshal.FreeCoTaskMem(key_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_LoadMetadata")]
    private static extern ulong ovr_CloudStorage_LoadMetadata_Native(IntPtr bucket, IntPtr key);

    public static ulong ovr_CloudStorage_ResolveKeepLocal(string bucket, string key, string remoteHandle) {
      IntPtr bucket_native = StringToNative(bucket);
      IntPtr key_native = StringToNative(key);
      IntPtr remoteHandle_native = StringToNative(remoteHandle);
      var result = (ovr_CloudStorage_ResolveKeepLocal_Native(bucket_native, key_native, remoteHandle_native));
      Marshal.FreeCoTaskMem(bucket_native);
      Marshal.FreeCoTaskMem(key_native);
      Marshal.FreeCoTaskMem(remoteHandle_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_ResolveKeepLocal")]
    private static extern ulong ovr_CloudStorage_ResolveKeepLocal_Native(IntPtr bucket, IntPtr key, IntPtr remoteHandle);

    public static ulong ovr_CloudStorage_ResolveKeepRemote(string bucket, string key, string remoteHandle) {
      IntPtr bucket_native = StringToNative(bucket);
      IntPtr key_native = StringToNative(key);
      IntPtr remoteHandle_native = StringToNative(remoteHandle);
      var result = (ovr_CloudStorage_ResolveKeepRemote_Native(bucket_native, key_native, remoteHandle_native));
      Marshal.FreeCoTaskMem(bucket_native);
      Marshal.FreeCoTaskMem(key_native);
      Marshal.FreeCoTaskMem(remoteHandle_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_ResolveKeepRemote")]
    private static extern ulong ovr_CloudStorage_ResolveKeepRemote_Native(IntPtr bucket, IntPtr key, IntPtr remoteHandle);

    public static ulong ovr_CloudStorage_Save(string bucket, string key, byte[] data, uint dataSize, long counter, string extraData) {
      IntPtr bucket_native = StringToNative(bucket);
      IntPtr key_native = StringToNative(key);
      IntPtr extraData_native = StringToNative(extraData);
      var result = (ovr_CloudStorage_Save_Native(bucket_native, key_native, data, dataSize, counter, extraData_native));
      Marshal.FreeCoTaskMem(bucket_native);
      Marshal.FreeCoTaskMem(key_native);
      Marshal.FreeCoTaskMem(extraData_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorage_Save")]
    private static extern ulong ovr_CloudStorage_Save_Native(IntPtr bucket, IntPtr key, byte[] data, uint dataSize, long counter, IntPtr extraData);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Entitlement_GetIsViewerEntitled();

    public static ulong ovr_GraphAPI_Get(string url) {
      IntPtr url_native = StringToNative(url);
      var result = (ovr_GraphAPI_Get_Native(url_native));
      Marshal.FreeCoTaskMem(url_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_GraphAPI_Get")]
    private static extern ulong ovr_GraphAPI_Get_Native(IntPtr url);

    public static ulong ovr_GraphAPI_Post(string url) {
      IntPtr url_native = StringToNative(url);
      var result = (ovr_GraphAPI_Post_Native(url_native));
      Marshal.FreeCoTaskMem(url_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_GraphAPI_Post")]
    private static extern ulong ovr_GraphAPI_Post_Native(IntPtr url);

    public static ulong ovr_HTTP_Get(string url) {
      IntPtr url_native = StringToNative(url);
      var result = (ovr_HTTP_Get_Native(url_native));
      Marshal.FreeCoTaskMem(url_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_HTTP_Get")]
    private static extern ulong ovr_HTTP_Get_Native(IntPtr url);

    public static ulong ovr_HTTP_GetToFile(string url, string diskFile) {
      IntPtr url_native = StringToNative(url);
      IntPtr diskFile_native = StringToNative(diskFile);
      var result = (ovr_HTTP_GetToFile_Native(url_native, diskFile_native));
      Marshal.FreeCoTaskMem(url_native);
      Marshal.FreeCoTaskMem(diskFile_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_HTTP_GetToFile")]
    private static extern ulong ovr_HTTP_GetToFile_Native(IntPtr url, IntPtr diskFile);

    public static ulong ovr_HTTP_Post(string url) {
      IntPtr url_native = StringToNative(url);
      var result = (ovr_HTTP_Post_Native(url_native));
      Marshal.FreeCoTaskMem(url_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_HTTP_Post")]
    private static extern ulong ovr_HTTP_Post_Native(IntPtr url);

    public static ulong ovr_IAP_ConsumePurchase(string sku) {
      IntPtr sku_native = StringToNative(sku);
      var result = (ovr_IAP_ConsumePurchase_Native(sku_native));
      Marshal.FreeCoTaskMem(sku_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_IAP_ConsumePurchase")]
    private static extern ulong ovr_IAP_ConsumePurchase_Native(IntPtr sku);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_IAP_GetProductsBySKU(string[] skus, int count);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_IAP_GetViewerPurchases();

    public static ulong ovr_IAP_LaunchCheckoutFlow(string sku) {
      IntPtr sku_native = StringToNative(sku);
      var result = (ovr_IAP_LaunchCheckoutFlow_Native(sku_native));
      Marshal.FreeCoTaskMem(sku_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_IAP_LaunchCheckoutFlow")]
    private static extern ulong ovr_IAP_LaunchCheckoutFlow_Native(IntPtr sku);

    public static ulong ovr_Leaderboard_GetEntries(string leaderboardName, int limit, LeaderboardFilterType filter, LeaderboardStartAt startAt) {
      IntPtr leaderboardName_native = StringToNative(leaderboardName);
      var result = (ovr_Leaderboard_GetEntries_Native(leaderboardName_native, limit, filter, startAt));
      Marshal.FreeCoTaskMem(leaderboardName_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Leaderboard_GetEntries")]
    private static extern ulong ovr_Leaderboard_GetEntries_Native(IntPtr leaderboardName, int limit, LeaderboardFilterType filter, LeaderboardStartAt startAt);

    public static ulong ovr_Leaderboard_GetEntriesAfterRank(string leaderboardName, int limit, ulong afterRank) {
      IntPtr leaderboardName_native = StringToNative(leaderboardName);
      var result = (ovr_Leaderboard_GetEntriesAfterRank_Native(leaderboardName_native, limit, afterRank));
      Marshal.FreeCoTaskMem(leaderboardName_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Leaderboard_GetEntriesAfterRank")]
    private static extern ulong ovr_Leaderboard_GetEntriesAfterRank_Native(IntPtr leaderboardName, int limit, ulong afterRank);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Leaderboard_GetNextEntries(IntPtr handle);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Leaderboard_GetPreviousEntries(IntPtr handle);

    public static ulong ovr_Leaderboard_WriteEntry(string leaderboardName, long score, byte[] extraData, uint extraDataLength, bool forceUpdate) {
      IntPtr leaderboardName_native = StringToNative(leaderboardName);
      var result = (ovr_Leaderboard_WriteEntry_Native(leaderboardName_native, score, extraData, extraDataLength, forceUpdate));
      Marshal.FreeCoTaskMem(leaderboardName_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Leaderboard_WriteEntry")]
    private static extern ulong ovr_Leaderboard_WriteEntry_Native(IntPtr leaderboardName, long score, byte[] extraData, uint extraDataLength, bool forceUpdate);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Livestreaming_GetStatus();

    public static ulong ovr_Livestreaming_IsAllowedForApplication(string packageName) {
      IntPtr packageName_native = StringToNative(packageName);
      var result = (ovr_Livestreaming_IsAllowedForApplication_Native(packageName_native));
      Marshal.FreeCoTaskMem(packageName_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Livestreaming_IsAllowedForApplication")]
    private static extern ulong ovr_Livestreaming_IsAllowedForApplication_Native(IntPtr packageName);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Livestreaming_PauseStream();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Livestreaming_ResumeStream();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Livestreaming_StartStream(LivestreamingAudience audience, LivestreamingMicrophoneStatus micStatus);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Livestreaming_StopStream();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Livestreaming_UpdateMicStatus(LivestreamingMicrophoneStatus micStatus);

    public static ulong ovr_Matchmaking_Browse(string pool, IntPtr customQueryData) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_Browse_Native(pool_native, customQueryData));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_Browse")]
    private static extern ulong ovr_Matchmaking_Browse_Native(IntPtr pool, IntPtr customQueryData);

    public static ulong ovr_Matchmaking_Browse2(string pool, IntPtr matchmakingOptions) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_Browse2_Native(pool_native, matchmakingOptions));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_Browse2")]
    private static extern ulong ovr_Matchmaking_Browse2_Native(IntPtr pool, IntPtr matchmakingOptions);

    public static ulong ovr_Matchmaking_Cancel(string pool, string requestHash) {
      IntPtr pool_native = StringToNative(pool);
      IntPtr requestHash_native = StringToNative(requestHash);
      var result = (ovr_Matchmaking_Cancel_Native(pool_native, requestHash_native));
      Marshal.FreeCoTaskMem(pool_native);
      Marshal.FreeCoTaskMem(requestHash_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_Cancel")]
    private static extern ulong ovr_Matchmaking_Cancel_Native(IntPtr pool, IntPtr requestHash);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Matchmaking_Cancel2();

    public static ulong ovr_Matchmaking_CreateAndEnqueueRoom(string pool, uint maxUsers, bool subscribeToUpdates, IntPtr customQueryData) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_CreateAndEnqueueRoom_Native(pool_native, maxUsers, subscribeToUpdates, customQueryData));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_CreateAndEnqueueRoom")]
    private static extern ulong ovr_Matchmaking_CreateAndEnqueueRoom_Native(IntPtr pool, uint maxUsers, bool subscribeToUpdates, IntPtr customQueryData);

    public static ulong ovr_Matchmaking_CreateAndEnqueueRoom2(string pool, IntPtr matchmakingOptions) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_CreateAndEnqueueRoom2_Native(pool_native, matchmakingOptions));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_CreateAndEnqueueRoom2")]
    private static extern ulong ovr_Matchmaking_CreateAndEnqueueRoom2_Native(IntPtr pool, IntPtr matchmakingOptions);

    public static ulong ovr_Matchmaking_CreateRoom(string pool, uint maxUsers, bool subscribeToUpdates) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_CreateRoom_Native(pool_native, maxUsers, subscribeToUpdates));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_CreateRoom")]
    private static extern ulong ovr_Matchmaking_CreateRoom_Native(IntPtr pool, uint maxUsers, bool subscribeToUpdates);

    public static ulong ovr_Matchmaking_CreateRoom2(string pool, IntPtr matchmakingOptions) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_CreateRoom2_Native(pool_native, matchmakingOptions));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_CreateRoom2")]
    private static extern ulong ovr_Matchmaking_CreateRoom2_Native(IntPtr pool, IntPtr matchmakingOptions);

    public static ulong ovr_Matchmaking_Enqueue(string pool, IntPtr customQueryData) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_Enqueue_Native(pool_native, customQueryData));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_Enqueue")]
    private static extern ulong ovr_Matchmaking_Enqueue_Native(IntPtr pool, IntPtr customQueryData);

    public static ulong ovr_Matchmaking_Enqueue2(string pool, IntPtr matchmakingOptions) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_Enqueue2_Native(pool_native, matchmakingOptions));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_Enqueue2")]
    private static extern ulong ovr_Matchmaking_Enqueue2_Native(IntPtr pool, IntPtr matchmakingOptions);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Matchmaking_EnqueueRoom(UInt64 roomID, IntPtr customQueryData);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Matchmaking_EnqueueRoom2(UInt64 roomID, IntPtr matchmakingOptions);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Matchmaking_GetAdminSnapshot();

    public static ulong ovr_Matchmaking_GetStats(string pool, uint maxLevel, MatchmakingStatApproach approach) {
      IntPtr pool_native = StringToNative(pool);
      var result = (ovr_Matchmaking_GetStats_Native(pool_native, maxLevel, approach));
      Marshal.FreeCoTaskMem(pool_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_GetStats")]
    private static extern ulong ovr_Matchmaking_GetStats_Native(IntPtr pool, uint maxLevel, MatchmakingStatApproach approach);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Matchmaking_JoinRoom(UInt64 roomID, bool subscribeToUpdates);

    public static ulong ovr_Matchmaking_ReportResultInsecure(UInt64 roomID, ovrKeyValuePair[] data) {
      UIntPtr data_length = (UIntPtr)data.Length;
      var result = (ovr_Matchmaking_ReportResultInsecure_Native(roomID, data, data_length));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Matchmaking_ReportResultInsecure")]
    private static extern ulong ovr_Matchmaking_ReportResultInsecure_Native(UInt64 roomID, ovrKeyValuePair[] data, UIntPtr numItems);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Matchmaking_StartMatch(UInt64 roomID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Notification_GetRoomInvites();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Notification_MarkAsRead(UInt64 notificationID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_Create();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_GatherInApplication(UInt64 partyID, UInt64 appID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_Get(UInt64 partyID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_GetCurrent();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_GetCurrentForUser(UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_Invite(UInt64 partyID, UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_Join(UInt64 partyID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Party_Leave(UInt64 partyID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_CreateAndJoinPrivate(RoomJoinPolicy joinPolicy, uint maxUsers, bool subscribeToUpdates);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_CreateAndJoinPrivate2(RoomJoinPolicy joinPolicy, uint maxUsers, IntPtr roomOptions);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_Get(UInt64 roomID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_GetCurrent();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_GetCurrentForUser(UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_GetInvitableUsers();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_GetInvitableUsers2(IntPtr roomOptions);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_GetModeratedRooms();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_GetSocialRooms(UInt64 appID);

    public static ulong ovr_Room_InviteUser(UInt64 roomID, string inviteToken) {
      IntPtr inviteToken_native = StringToNative(inviteToken);
      var result = (ovr_Room_InviteUser_Native(roomID, inviteToken_native));
      Marshal.FreeCoTaskMem(inviteToken_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Room_InviteUser")]
    private static extern ulong ovr_Room_InviteUser_Native(UInt64 roomID, IntPtr inviteToken);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_Join(UInt64 roomID, bool subscribeToUpdates);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_Join2(UInt64 roomID, IntPtr roomOptions);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_KickUser(UInt64 roomID, UInt64 userID, int kickDurationSeconds);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_LaunchInvitableUserFlow(UInt64 roomID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_Leave(UInt64 roomID);

    public static ulong ovr_Room_SetDescription(UInt64 roomID, string description) {
      IntPtr description_native = StringToNative(description);
      var result = (ovr_Room_SetDescription_Native(roomID, description_native));
      Marshal.FreeCoTaskMem(description_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Room_SetDescription")]
    private static extern ulong ovr_Room_SetDescription_Native(UInt64 roomID, IntPtr description);

    public static ulong ovr_Room_UpdateDataStore(UInt64 roomID, ovrKeyValuePair[] data) {
      UIntPtr data_length = (UIntPtr)data.Length;
      var result = (ovr_Room_UpdateDataStore_Native(roomID, data, data_length));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Room_UpdateDataStore")]
    private static extern ulong ovr_Room_UpdateDataStore_Native(UInt64 roomID, ovrKeyValuePair[] data, UIntPtr numItems);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_UpdateMembershipLockStatus(UInt64 roomID, RoomMembershipLockStatus membershipLockStatus);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_UpdateOwner(UInt64 roomID, UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Room_UpdatePrivateRoomJoinPolicy(UInt64 roomID, RoomJoinPolicy newJoinPolicy);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_SystemPermissions_GetStatus(PermissionType permType);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_SystemPermissions_LaunchDeeplink(PermissionType permType);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_Get(UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_GetAccessToken();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_GetLoggedInUser();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_GetLoggedInUserFriends();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_GetLoggedInUserFriendsAndRooms();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_GetOrgScopedID(UInt64 userID);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_GetUserProof();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_NewEntitledTestUser();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_NewTestUser();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_User_NewTestUserFriends();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Voip_SetSystemVoipSuppressed(bool suppressed);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_AchievementDefinition_GetBitfieldLength(IntPtr obj);

    public static string ovr_AchievementDefinition_GetName(IntPtr obj) {
      var result = StringFromNative(ovr_AchievementDefinition_GetName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_AchievementDefinition_GetName")]
    private static extern IntPtr ovr_AchievementDefinition_GetName_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_AchievementDefinition_GetTarget(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern AchievementType ovr_AchievementDefinition_GetType(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_AchievementDefinitionArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_AchievementDefinitionArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_AchievementDefinitionArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_AchievementDefinitionArray_GetNextUrl")]
    private static extern IntPtr ovr_AchievementDefinitionArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_AchievementDefinitionArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_AchievementDefinitionArray_HasNextPage(IntPtr obj);

    public static string ovr_AchievementProgress_GetBitfield(IntPtr obj) {
      var result = StringFromNative(ovr_AchievementProgress_GetBitfield_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_AchievementProgress_GetBitfield")]
    private static extern IntPtr ovr_AchievementProgress_GetBitfield_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_AchievementProgress_GetCount(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_AchievementProgress_GetIsUnlocked(IntPtr obj);

    public static string ovr_AchievementProgress_GetName(IntPtr obj) {
      var result = StringFromNative(ovr_AchievementProgress_GetName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_AchievementProgress_GetName")]
    private static extern IntPtr ovr_AchievementProgress_GetName_Native(IntPtr obj);

    public static DateTime ovr_AchievementProgress_GetUnlockTime(IntPtr obj) {
      var result = DateTimeFromNative(ovr_AchievementProgress_GetUnlockTime_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_AchievementProgress_GetUnlockTime")]
    private static extern ulong ovr_AchievementProgress_GetUnlockTime_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_AchievementProgressArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_AchievementProgressArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_AchievementProgressArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_AchievementProgressArray_GetNextUrl")]
    private static extern IntPtr ovr_AchievementProgressArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_AchievementProgressArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_AchievementProgressArray_HasNextPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_AchievementUpdate_GetJustUnlocked(IntPtr obj);

    public static string ovr_AchievementUpdate_GetName(IntPtr obj) {
      var result = StringFromNative(ovr_AchievementUpdate_GetName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_AchievementUpdate_GetName")]
    private static extern IntPtr ovr_AchievementUpdate_GetName_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_Application_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_ApplicationVersion_GetCurrentCode(IntPtr obj);

    public static string ovr_ApplicationVersion_GetCurrentName(IntPtr obj) {
      var result = StringFromNative(ovr_ApplicationVersion_GetCurrentName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_ApplicationVersion_GetCurrentName")]
    private static extern IntPtr ovr_ApplicationVersion_GetCurrentName_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_ApplicationVersion_GetLatestCode(IntPtr obj);

    public static string ovr_ApplicationVersion_GetLatestName(IntPtr obj) {
      var result = StringFromNative(ovr_ApplicationVersion_GetLatestName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_ApplicationVersion_GetLatestName")]
    private static extern IntPtr ovr_ApplicationVersion_GetLatestName_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_CloudStorageConflictMetadata_GetLocal(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_CloudStorageConflictMetadata_GetRemote(IntPtr obj);

    public static string ovr_CloudStorageData_GetBucket(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageData_GetBucket_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageData_GetBucket")]
    private static extern IntPtr ovr_CloudStorageData_GetBucket_Native(IntPtr obj);

    public static byte[] ovr_CloudStorageData_GetData(IntPtr obj) {
      var result = FiledataFromNative(ovr_CloudStorageData_GetDataSize(obj), ovr_CloudStorageData_GetData_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageData_GetData")]
    private static extern IntPtr ovr_CloudStorageData_GetData_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_CloudStorageData_GetDataSize(IntPtr obj);

    public static string ovr_CloudStorageData_GetKey(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageData_GetKey_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageData_GetKey")]
    private static extern IntPtr ovr_CloudStorageData_GetKey_Native(IntPtr obj);

    public static string ovr_CloudStorageMetadata_GetBucket(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageMetadata_GetBucket_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageMetadata_GetBucket")]
    private static extern IntPtr ovr_CloudStorageMetadata_GetBucket_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern long ovr_CloudStorageMetadata_GetCounter(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_CloudStorageMetadata_GetDataSize(IntPtr obj);

    public static string ovr_CloudStorageMetadata_GetExtraData(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageMetadata_GetExtraData_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageMetadata_GetExtraData")]
    private static extern IntPtr ovr_CloudStorageMetadata_GetExtraData_Native(IntPtr obj);

    public static string ovr_CloudStorageMetadata_GetKey(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageMetadata_GetKey_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageMetadata_GetKey")]
    private static extern IntPtr ovr_CloudStorageMetadata_GetKey_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_CloudStorageMetadata_GetSaveTime(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern CloudStorageDataStatus ovr_CloudStorageMetadata_GetStatus(IntPtr obj);

    public static string ovr_CloudStorageMetadata_GetVersionHandle(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageMetadata_GetVersionHandle_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageMetadata_GetVersionHandle")]
    private static extern IntPtr ovr_CloudStorageMetadata_GetVersionHandle_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_CloudStorageMetadataArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_CloudStorageMetadataArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageMetadataArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageMetadataArray_GetNextUrl")]
    private static extern IntPtr ovr_CloudStorageMetadataArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_CloudStorageMetadataArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_CloudStorageMetadataArray_HasNextPage(IntPtr obj);

    public static string ovr_CloudStorageUpdateResponse_GetBucket(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageUpdateResponse_GetBucket_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageUpdateResponse_GetBucket")]
    private static extern IntPtr ovr_CloudStorageUpdateResponse_GetBucket_Native(IntPtr obj);

    public static string ovr_CloudStorageUpdateResponse_GetKey(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageUpdateResponse_GetKey_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageUpdateResponse_GetKey")]
    private static extern IntPtr ovr_CloudStorageUpdateResponse_GetKey_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern CloudStorageUpdateStatus ovr_CloudStorageUpdateResponse_GetStatus(IntPtr obj);

    public static string ovr_CloudStorageUpdateResponse_GetVersionHandle(IntPtr obj) {
      var result = StringFromNative(ovr_CloudStorageUpdateResponse_GetVersionHandle_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_CloudStorageUpdateResponse_GetVersionHandle")]
    private static extern IntPtr ovr_CloudStorageUpdateResponse_GetVersionHandle_Native(IntPtr obj);

    public static uint ovr_DataStore_Contains(IntPtr obj, string key) {
      IntPtr key_native = StringToNative(key);
      var result = (ovr_DataStore_Contains_Native(obj, key_native));
      Marshal.FreeCoTaskMem(key_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_DataStore_Contains")]
    private static extern uint ovr_DataStore_Contains_Native(IntPtr obj, IntPtr key);

    public static string ovr_DataStore_GetKey(IntPtr obj, int index) {
      var result = StringFromNative(ovr_DataStore_GetKey_Native(obj, index));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_DataStore_GetKey")]
    private static extern IntPtr ovr_DataStore_GetKey_Native(IntPtr obj, int index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_DataStore_GetNumKeys(IntPtr obj);

    public static string ovr_DataStore_GetValue(IntPtr obj, string key) {
      IntPtr key_native = StringToNative(key);
      var result = StringFromNative(ovr_DataStore_GetValue_Native(obj, key_native));
      Marshal.FreeCoTaskMem(key_native);
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_DataStore_GetValue")]
    private static extern IntPtr ovr_DataStore_GetValue_Native(IntPtr obj, IntPtr key);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_Error_GetCode(IntPtr obj);

    public static string ovr_Error_GetDisplayableMessage(IntPtr obj) {
      var result = StringFromNative(ovr_Error_GetDisplayableMessage_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Error_GetDisplayableMessage")]
    private static extern IntPtr ovr_Error_GetDisplayableMessage_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_Error_GetHttpCode(IntPtr obj);

    public static string ovr_Error_GetMessage(IntPtr obj) {
      var result = StringFromNative(ovr_Error_GetMessage_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Error_GetMessage")]
    private static extern IntPtr ovr_Error_GetMessage_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_HttpTransferUpdate_GetBytes(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_HttpTransferUpdate_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_HttpTransferUpdate_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_HttpTransferUpdate_IsCompleted(IntPtr obj);

    public static string ovr_InstalledApplication_GetApplicationId(IntPtr obj) {
      var result = StringFromNative(ovr_InstalledApplication_GetApplicationId_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_InstalledApplication_GetApplicationId")]
    private static extern IntPtr ovr_InstalledApplication_GetApplicationId_Native(IntPtr obj);

    public static string ovr_InstalledApplication_GetPackageName(IntPtr obj) {
      var result = StringFromNative(ovr_InstalledApplication_GetPackageName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_InstalledApplication_GetPackageName")]
    private static extern IntPtr ovr_InstalledApplication_GetPackageName_Native(IntPtr obj);

    public static string ovr_InstalledApplication_GetStatus(IntPtr obj) {
      var result = StringFromNative(ovr_InstalledApplication_GetStatus_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_InstalledApplication_GetStatus")]
    private static extern IntPtr ovr_InstalledApplication_GetStatus_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_InstalledApplication_GetVersionCode(IntPtr obj);

    public static string ovr_InstalledApplication_GetVersionName(IntPtr obj) {
      var result = StringFromNative(ovr_InstalledApplication_GetVersionName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_InstalledApplication_GetVersionName")]
    private static extern IntPtr ovr_InstalledApplication_GetVersionName_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_InstalledApplicationArray_GetElement(IntPtr obj, UIntPtr index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_InstalledApplicationArray_GetSize(IntPtr obj);

    public static string ovr_LaunchDetails_GetDeeplinkMessage(IntPtr obj) {
      var result = StringFromNative(ovr_LaunchDetails_GetDeeplinkMessage_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_LaunchDetails_GetDeeplinkMessage")]
    private static extern IntPtr ovr_LaunchDetails_GetDeeplinkMessage_Native(IntPtr obj);

    public static string ovr_LaunchDetails_GetLaunchSource(IntPtr obj) {
      var result = StringFromNative(ovr_LaunchDetails_GetLaunchSource_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_LaunchDetails_GetLaunchSource")]
    private static extern IntPtr ovr_LaunchDetails_GetLaunchSource_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern LaunchType ovr_LaunchDetails_GetLaunchType(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_LaunchDetails_GetRoomID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_LaunchDetails_GetUsers(IntPtr obj);

    public static byte[] ovr_LeaderboardEntry_GetExtraData(IntPtr obj) {
      var result = BlobFromNative(ovr_LeaderboardEntry_GetExtraDataLength(obj), ovr_LeaderboardEntry_GetExtraData_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_LeaderboardEntry_GetExtraData")]
    private static extern IntPtr ovr_LeaderboardEntry_GetExtraData_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_LeaderboardEntry_GetExtraDataLength(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_LeaderboardEntry_GetRank(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern long ovr_LeaderboardEntry_GetScore(IntPtr obj);

    public static DateTime ovr_LeaderboardEntry_GetTimestamp(IntPtr obj) {
      var result = DateTimeFromNative(ovr_LeaderboardEntry_GetTimestamp_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_LeaderboardEntry_GetTimestamp")]
    private static extern ulong ovr_LeaderboardEntry_GetTimestamp_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_LeaderboardEntry_GetUser(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_LeaderboardEntryArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_LeaderboardEntryArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_LeaderboardEntryArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_LeaderboardEntryArray_GetNextUrl")]
    private static extern IntPtr ovr_LeaderboardEntryArray_GetNextUrl_Native(IntPtr obj);

    public static string ovr_LeaderboardEntryArray_GetPreviousUrl(IntPtr obj) {
      var result = StringFromNative(ovr_LeaderboardEntryArray_GetPreviousUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_LeaderboardEntryArray_GetPreviousUrl")]
    private static extern IntPtr ovr_LeaderboardEntryArray_GetPreviousUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_LeaderboardEntryArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_LeaderboardEntryArray_GetTotalCount(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_LeaderboardEntryArray_HasNextPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_LeaderboardEntryArray_HasPreviousPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_LeaderboardUpdateStatus_GetDidUpdate(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_LivestreamingApplicationStatus_GetStreamingEnabled(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern LivestreamingStartStatus ovr_LivestreamingStartResult_GetStreamingResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_LivestreamingStatus_GetIsPaused(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_LivestreamingStatus_GetLivestreamingEnabled(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_LivestreamingStatus_GetMicEnabled(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_LivestreamingVideoStats_GetCommentCount(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern int ovr_LivestreamingVideoStats_GetReactionCount(IntPtr obj);

    public static string ovr_LivestreamingVideoStats_GetTotalViews(IntPtr obj) {
      var result = StringFromNative(ovr_LivestreamingVideoStats_GetTotalViews_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_LivestreamingVideoStats_GetTotalViews")]
    private static extern IntPtr ovr_LivestreamingVideoStats_GetTotalViews_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingAdminSnapshot_GetCandidates(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern double ovr_MatchmakingAdminSnapshot_GetMyCurrentThreshold(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_MatchmakingAdminSnapshotCandidate_GetCanMatch(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern double ovr_MatchmakingAdminSnapshotCandidate_GetMyTotalScore(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern double ovr_MatchmakingAdminSnapshotCandidate_GetTheirCurrentThreshold(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern double ovr_MatchmakingAdminSnapshotCandidate_GetTheirTotalScore(IntPtr obj);

    public static string ovr_MatchmakingAdminSnapshotCandidate_GetTraceId(IntPtr obj) {
      var result = StringFromNative(ovr_MatchmakingAdminSnapshotCandidate_GetTraceId_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingAdminSnapshotCandidate_GetTraceId")]
    private static extern IntPtr ovr_MatchmakingAdminSnapshotCandidate_GetTraceId_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingAdminSnapshotCandidateArray_GetElement(IntPtr obj, UIntPtr index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_MatchmakingAdminSnapshotCandidateArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingBrowseResult_GetEnqueueResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingBrowseResult_GetRooms(IntPtr obj);

    public static string ovr_MatchmakingCandidate_GetEntryHash(IntPtr obj) {
      var result = StringFromNative(ovr_MatchmakingCandidate_GetEntryHash_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingCandidate_GetEntryHash")]
    private static extern IntPtr ovr_MatchmakingCandidate_GetEntryHash_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_MatchmakingCandidate_GetUserId(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingCandidateArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_MatchmakingCandidateArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_MatchmakingCandidateArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingCandidateArray_GetNextUrl")]
    private static extern IntPtr ovr_MatchmakingCandidateArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_MatchmakingCandidateArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_MatchmakingCandidateArray_HasNextPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingEnqueueResult_GetAdminSnapshot(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingEnqueueResult_GetAverageWait(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingEnqueueResult_GetMatchesInLastHourCount(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingEnqueueResult_GetMaxExpectedWait(IntPtr obj);

    public static string ovr_MatchmakingEnqueueResult_GetPool(IntPtr obj) {
      var result = StringFromNative(ovr_MatchmakingEnqueueResult_GetPool_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingEnqueueResult_GetPool")]
    private static extern IntPtr ovr_MatchmakingEnqueueResult_GetPool_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingEnqueueResult_GetRecentMatchPercentage(IntPtr obj);

    public static string ovr_MatchmakingEnqueueResult_GetRequestHash(IntPtr obj) {
      var result = StringFromNative(ovr_MatchmakingEnqueueResult_GetRequestHash_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingEnqueueResult_GetRequestHash")]
    private static extern IntPtr ovr_MatchmakingEnqueueResult_GetRequestHash_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingEnqueueResultAndRoom_GetMatchmakingEnqueueResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingEnqueueResultAndRoom_GetRoom(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_MatchmakingEnqueuedUser_GetAdditionalUserID(IntPtr obj, uint index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingEnqueuedUser_GetAdditionalUserIDsSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingEnqueuedUser_GetCustomData(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingEnqueuedUser_GetUser(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingEnqueuedUserArray_GetElement(IntPtr obj, UIntPtr index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_MatchmakingEnqueuedUserArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_MatchmakingNotification_GetAddedByUserId(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingNotification_GetRoom(IntPtr obj);

    public static string ovr_MatchmakingNotification_GetTraceId(IntPtr obj) {
      var result = StringFromNative(ovr_MatchmakingNotification_GetTraceId_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingNotification_GetTraceId")]
    private static extern IntPtr ovr_MatchmakingNotification_GetTraceId_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingRoom_GetPingTime(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingRoom_GetRoom(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_MatchmakingRoom_HasPingTime(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingRoomArray_GetElement(IntPtr obj, UIntPtr index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_MatchmakingRoomArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingStats_GetDrawCount(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingStats_GetLossCount(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingStats_GetSkillLevel(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_MatchmakingStats_GetWinCount(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetAchievementDefinitionArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetAchievementProgressArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetAchievementUpdate(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetApplicationVersion(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetCloudStorageConflictMetadata(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetCloudStorageData(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetCloudStorageMetadata(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetCloudStorageMetadataArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetCloudStorageUpdateResponse(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetError(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetHttpTransferUpdate(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetInstalledApplicationArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetLeaderboardEntryArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetLeaderboardUpdateStatus(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetLivestreamingApplicationStatus(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetLivestreamingStartResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetLivestreamingStatus(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetLivestreamingVideoStats(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetMatchmakingAdminSnapshot(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetMatchmakingBrowseResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetMatchmakingEnqueueResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetMatchmakingEnqueueResultAndRoom(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetMatchmakingRoomArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetMatchmakingStats(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetNativeMessage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetNetworkingPeer(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetOrgScopedID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetParty(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetPartyID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetPidArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetPingResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetPlatformInitialize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetProductArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetPurchase(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetPurchaseArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_Message_GetRequestID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetRoom(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetRoomArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetRoomInviteNotificationArray(IntPtr obj);

    public static string ovr_Message_GetString(IntPtr obj) {
      var result = StringFromNative(ovr_Message_GetString_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Message_GetString")]
    private static extern IntPtr ovr_Message_GetString_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetSystemPermission(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetSystemVoipState(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern Message.MessageType ovr_Message_GetType(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetUser(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetUserAndRoomArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetUserArray(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Message_GetUserProof(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_Message_IsError(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Microphone_GetNumSamplesAvailable(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Microphone_GetOutputBufferMaxSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Microphone_GetPCM(IntPtr obj, Int16[] outputBuffer, UIntPtr outputBufferNumElements);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Microphone_GetPCMFloat(IntPtr obj, float[] outputBuffer, UIntPtr outputBufferNumElements);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Microphone_ReadData(IntPtr obj, float[] outputBuffer, UIntPtr outputBufferSize);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Microphone_SetAcceptableRecordingDelayHint(IntPtr obj, UIntPtr delayMs);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Microphone_Start(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Microphone_Stop(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_NetworkingPeer_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern PeerConnectionState ovr_NetworkingPeer_GetState(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_OrgScopedID_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_Packet_Free(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Packet_GetBytes(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern SendPolicy ovr_Packet_GetSendPolicy(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_Packet_GetSenderID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_Packet_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_Party_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Party_GetInvitedUsers(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Party_GetLeader(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Party_GetRoom(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Party_GetUsers(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_PartyID_GetID(IntPtr obj);

    public static string ovr_Pid_GetId(IntPtr obj) {
      var result = StringFromNative(ovr_Pid_GetId_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Pid_GetId")]
    private static extern IntPtr ovr_Pid_GetId_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_PidArray_GetElement(IntPtr obj, UIntPtr index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_PidArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_PingResult_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern ulong ovr_PingResult_GetPingTimeUsec(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_PingResult_IsTimeout(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern PlatformInitializeResult ovr_PlatformInitialize_GetResult(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_Price_GetAmountInHundredths(IntPtr obj);

    public static string ovr_Price_GetCurrency(IntPtr obj) {
      var result = StringFromNative(ovr_Price_GetCurrency_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Price_GetCurrency")]
    private static extern IntPtr ovr_Price_GetCurrency_Native(IntPtr obj);

    public static string ovr_Price_GetFormatted(IntPtr obj) {
      var result = StringFromNative(ovr_Price_GetFormatted_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Price_GetFormatted")]
    private static extern IntPtr ovr_Price_GetFormatted_Native(IntPtr obj);

    public static string ovr_Product_GetDescription(IntPtr obj) {
      var result = StringFromNative(ovr_Product_GetDescription_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Product_GetDescription")]
    private static extern IntPtr ovr_Product_GetDescription_Native(IntPtr obj);

    public static string ovr_Product_GetFormattedPrice(IntPtr obj) {
      var result = StringFromNative(ovr_Product_GetFormattedPrice_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Product_GetFormattedPrice")]
    private static extern IntPtr ovr_Product_GetFormattedPrice_Native(IntPtr obj);

    public static string ovr_Product_GetName(IntPtr obj) {
      var result = StringFromNative(ovr_Product_GetName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Product_GetName")]
    private static extern IntPtr ovr_Product_GetName_Native(IntPtr obj);

    public static string ovr_Product_GetSKU(IntPtr obj) {
      var result = StringFromNative(ovr_Product_GetSKU_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Product_GetSKU")]
    private static extern IntPtr ovr_Product_GetSKU_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_ProductArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_ProductArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_ProductArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_ProductArray_GetNextUrl")]
    private static extern IntPtr ovr_ProductArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_ProductArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_ProductArray_HasNextPage(IntPtr obj);

    public static DateTime ovr_Purchase_GetExpirationTime(IntPtr obj) {
      var result = DateTimeFromNative(ovr_Purchase_GetExpirationTime_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Purchase_GetExpirationTime")]
    private static extern ulong ovr_Purchase_GetExpirationTime_Native(IntPtr obj);

    public static DateTime ovr_Purchase_GetGrantTime(IntPtr obj) {
      var result = DateTimeFromNative(ovr_Purchase_GetGrantTime_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Purchase_GetGrantTime")]
    private static extern ulong ovr_Purchase_GetGrantTime_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_Purchase_GetPurchaseID(IntPtr obj);

    public static string ovr_Purchase_GetSKU(IntPtr obj) {
      var result = StringFromNative(ovr_Purchase_GetSKU_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Purchase_GetSKU")]
    private static extern IntPtr ovr_Purchase_GetSKU_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_PurchaseArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_PurchaseArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_PurchaseArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_PurchaseArray_GetNextUrl")]
    private static extern IntPtr ovr_PurchaseArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_PurchaseArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_PurchaseArray_HasNextPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_Room_GetApplicationID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Room_GetDataStore(IntPtr obj);

    public static string ovr_Room_GetDescription(IntPtr obj) {
      var result = StringFromNative(ovr_Room_GetDescription_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Room_GetDescription")]
    private static extern IntPtr ovr_Room_GetDescription_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_Room_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Room_GetInvitedUsers(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_Room_GetIsMembershipLocked(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern RoomJoinPolicy ovr_Room_GetJoinPolicy(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern RoomJoinability ovr_Room_GetJoinability(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Room_GetMatchedUsers(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_Room_GetMaxUsers(IntPtr obj);

    public static string ovr_Room_GetName(IntPtr obj) {
      var result = StringFromNative(ovr_Room_GetName_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_Room_GetName")]
    private static extern IntPtr ovr_Room_GetName_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Room_GetOwner(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern RoomType ovr_Room_GetType(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_Room_GetUsers(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern uint ovr_Room_GetVersion(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_RoomArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_RoomArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_RoomArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_RoomArray_GetNextUrl")]
    private static extern IntPtr ovr_RoomArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_RoomArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_RoomArray_HasNextPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_RoomInviteNotification_GetID(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_RoomInviteNotification_GetRoomID(IntPtr obj);

    public static DateTime ovr_RoomInviteNotification_GetSentTime(IntPtr obj) {
      var result = DateTimeFromNative(ovr_RoomInviteNotification_GetSentTime_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_RoomInviteNotification_GetSentTime")]
    private static extern ulong ovr_RoomInviteNotification_GetSentTime_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_RoomInviteNotificationArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_RoomInviteNotificationArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_RoomInviteNotificationArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_RoomInviteNotificationArray_GetNextUrl")]
    private static extern IntPtr ovr_RoomInviteNotificationArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_RoomInviteNotificationArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_RoomInviteNotificationArray_HasNextPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_SystemPermission_GetHasPermission(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern PermissionGrantStatus ovr_SystemPermission_GetPermissionGrantStatus(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern VoipMuteState ovr_SystemVoipState_GetMicrophoneMuted(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern SystemVoipStatus ovr_SystemVoipState_GetStatus(IntPtr obj);

    public static string ovr_TestUser_GetAccessToken(IntPtr obj) {
      var result = StringFromNative(ovr_TestUser_GetAccessToken_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_TestUser_GetAccessToken")]
    private static extern IntPtr ovr_TestUser_GetAccessToken_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_TestUser_GetAppAccessArray(IntPtr obj);

    public static string ovr_TestUser_GetFriendAccessToken(IntPtr obj) {
      var result = StringFromNative(ovr_TestUser_GetFriendAccessToken_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_TestUser_GetFriendAccessToken")]
    private static extern IntPtr ovr_TestUser_GetFriendAccessToken_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_TestUser_GetFriendAppAccessArray(IntPtr obj);

    public static string ovr_TestUser_GetUserAlias(IntPtr obj) {
      var result = StringFromNative(ovr_TestUser_GetUserAlias_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_TestUser_GetUserAlias")]
    private static extern IntPtr ovr_TestUser_GetUserAlias_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_TestUser_GetUserId(IntPtr obj);

    public static string ovr_TestUserAppAccess_GetAccessToken(IntPtr obj) {
      var result = StringFromNative(ovr_TestUserAppAccess_GetAccessToken_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_TestUserAppAccess_GetAccessToken")]
    private static extern IntPtr ovr_TestUserAppAccess_GetAccessToken_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_TestUserAppAccess_GetAppId(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_TestUserAppAccess_GetUserId(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_TestUserAppAccessArray_GetElement(IntPtr obj, UIntPtr index);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_TestUserAppAccessArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UInt64 ovr_User_GetID(IntPtr obj);

    public static string ovr_User_GetImageUrl(IntPtr obj) {
      var result = StringFromNative(ovr_User_GetImageUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_User_GetImageUrl")]
    private static extern IntPtr ovr_User_GetImageUrl_Native(IntPtr obj);

    public static string ovr_User_GetInviteToken(IntPtr obj) {
      var result = StringFromNative(ovr_User_GetInviteToken_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_User_GetInviteToken")]
    private static extern IntPtr ovr_User_GetInviteToken_Native(IntPtr obj);

    public static string ovr_User_GetOculusID(IntPtr obj) {
      var result = StringFromNative(ovr_User_GetOculusID_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_User_GetOculusID")]
    private static extern IntPtr ovr_User_GetOculusID_Native(IntPtr obj);

    public static string ovr_User_GetPresence(IntPtr obj) {
      var result = StringFromNative(ovr_User_GetPresence_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_User_GetPresence")]
    private static extern IntPtr ovr_User_GetPresence_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UserPresenceStatus ovr_User_GetPresenceStatus(IntPtr obj);

    public static string ovr_User_GetSmallImageUrl(IntPtr obj) {
      var result = StringFromNative(ovr_User_GetSmallImageUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_User_GetSmallImageUrl")]
    private static extern IntPtr ovr_User_GetSmallImageUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_UserAndRoom_GetRoom(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_UserAndRoom_GetUser(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_UserAndRoomArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_UserAndRoomArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_UserAndRoomArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_UserAndRoomArray_GetNextUrl")]
    private static extern IntPtr ovr_UserAndRoomArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_UserAndRoomArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_UserAndRoomArray_HasNextPage(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_UserArray_GetElement(IntPtr obj, UIntPtr index);

    public static string ovr_UserArray_GetNextUrl(IntPtr obj) {
      var result = StringFromNative(ovr_UserArray_GetNextUrl_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_UserArray_GetNextUrl")]
    private static extern IntPtr ovr_UserArray_GetNextUrl_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_UserArray_GetSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern bool ovr_UserArray_HasNextPage(IntPtr obj);

    public static string ovr_UserProof_GetNonce(IntPtr obj) {
      var result = StringFromNative(ovr_UserProof_GetNonce_Native(obj));
      return result;
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_UserProof_GetNonce")]
    private static extern IntPtr ovr_UserProof_GetNonce_Native(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_VoipDecoder_Decode(IntPtr obj, byte[] compressedData, UIntPtr compressedSize);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_VoipDecoder_GetDecodedPCM(IntPtr obj, float[] outputBuffer, UIntPtr outputBufferSize);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_VoipEncoder_AddPCM(IntPtr obj, float[] inputData, uint inputSize);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_VoipEncoder_GetCompressedData(IntPtr obj, byte[] outputBuffer, UIntPtr intputSize);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern UIntPtr ovr_VoipEncoder_GetCompressedDataSize(IntPtr obj);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_MatchmakingOptions_Create();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_Destroy(IntPtr handle);

    public static void ovr_MatchmakingOptions_SetCreateRoomDataStoreString(IntPtr handle, string key, string value) {
      IntPtr key_native = StringToNative(key);
      IntPtr value_native = StringToNative(value);
      ovr_MatchmakingOptions_SetCreateRoomDataStoreString_Native(handle, key_native, value_native);
      Marshal.FreeCoTaskMem(key_native);
      Marshal.FreeCoTaskMem(value_native);
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingOptions_SetCreateRoomDataStoreString")]
    private static extern void ovr_MatchmakingOptions_SetCreateRoomDataStoreString_Native(IntPtr handle, IntPtr key, IntPtr value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_ClearCreateRoomDataStore(IntPtr handle);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_SetCreateRoomJoinPolicy(IntPtr handle, RoomJoinPolicy value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_SetCreateRoomMaxUsers(IntPtr handle, uint value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_AddEnqueueAdditionalUser(IntPtr handle, UInt64 value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_ClearEnqueueAdditionalUsers(IntPtr handle);

    public static void ovr_MatchmakingOptions_SetEnqueueDataSettingsInt(IntPtr handle, string key, int value) {
      IntPtr key_native = StringToNative(key);
      ovr_MatchmakingOptions_SetEnqueueDataSettingsInt_Native(handle, key_native, value);
      Marshal.FreeCoTaskMem(key_native);
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingOptions_SetEnqueueDataSettingsInt")]
    private static extern void ovr_MatchmakingOptions_SetEnqueueDataSettingsInt_Native(IntPtr handle, IntPtr key, int value);

    public static void ovr_MatchmakingOptions_SetEnqueueDataSettingsDouble(IntPtr handle, string key, double value) {
      IntPtr key_native = StringToNative(key);
      ovr_MatchmakingOptions_SetEnqueueDataSettingsDouble_Native(handle, key_native, value);
      Marshal.FreeCoTaskMem(key_native);
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingOptions_SetEnqueueDataSettingsDouble")]
    private static extern void ovr_MatchmakingOptions_SetEnqueueDataSettingsDouble_Native(IntPtr handle, IntPtr key, double value);

    public static void ovr_MatchmakingOptions_SetEnqueueDataSettingsString(IntPtr handle, string key, string value) {
      IntPtr key_native = StringToNative(key);
      IntPtr value_native = StringToNative(value);
      ovr_MatchmakingOptions_SetEnqueueDataSettingsString_Native(handle, key_native, value_native);
      Marshal.FreeCoTaskMem(key_native);
      Marshal.FreeCoTaskMem(value_native);
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingOptions_SetEnqueueDataSettingsString")]
    private static extern void ovr_MatchmakingOptions_SetEnqueueDataSettingsString_Native(IntPtr handle, IntPtr key, IntPtr value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_ClearEnqueueDataSettings(IntPtr handle);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_MatchmakingOptions_SetEnqueueIsDebug(IntPtr handle, bool value);

    public static void ovr_MatchmakingOptions_SetEnqueueQueryKey(IntPtr handle, string value) {
      IntPtr value_native = StringToNative(value);
      ovr_MatchmakingOptions_SetEnqueueQueryKey_Native(handle, value_native);
      Marshal.FreeCoTaskMem(value_native);
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_MatchmakingOptions_SetEnqueueQueryKey")]
    private static extern void ovr_MatchmakingOptions_SetEnqueueQueryKey_Native(IntPtr handle, IntPtr value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern IntPtr ovr_RoomOptions_Create();

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_RoomOptions_Destroy(IntPtr handle);

    public static void ovr_RoomOptions_SetDataStoreString(IntPtr handle, string key, string value) {
      IntPtr key_native = StringToNative(key);
      IntPtr value_native = StringToNative(value);
      ovr_RoomOptions_SetDataStoreString_Native(handle, key_native, value_native);
      Marshal.FreeCoTaskMem(key_native);
      Marshal.FreeCoTaskMem(value_native);
    }

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl, EntryPoint="ovr_RoomOptions_SetDataStoreString")]
    private static extern void ovr_RoomOptions_SetDataStoreString_Native(IntPtr handle, IntPtr key, IntPtr value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_RoomOptions_ClearDataStore(IntPtr handle);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_RoomOptions_SetOrdering(IntPtr handle, UserOrdering value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_RoomOptions_SetRoomId(IntPtr handle, UInt64 value);

    [DllImport(DLL_NAME, CallingConvention=CallingConvention.Cdecl)]
    public static extern void ovr_RoomOptions_SetTurnOffUpdates(IntPtr handle, bool value);
  }
}
