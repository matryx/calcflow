namespace Oculus.Platform
{
  using UnityEngine;
  using System;
  using System.Collections;
  using System.Runtime.InteropServices;

  public sealed class StandalonePlatform
  {
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    public delegate void UnityLogDelegate(IntPtr tag, IntPtr msg);

    void CPPLogCallback(IntPtr tag, IntPtr message)
    {
      Debug.Log(string.Format("{0}: {1}", Marshal.PtrToStringAnsi(tag), Marshal.PtrToStringAnsi(message)));
    }

    public bool InitializeInEditor()
    {
      if (String.IsNullOrEmpty(StandalonePlatformSettings.OculusPlatformAccessToken))
      {
        throw new UnityException("Update your access token by selecting 'Oculus Platform' -> 'Edit Settings'");
      }
      return Initialize(StandalonePlatformSettings.OculusPlatformAccessToken);
    }

    public bool Initialize(string accessToken)
    {
      //UnityLogDelegate callback_delegate = new UnityLogDelegate(CPPLogCallback);
      //IntPtr intptr_delegate = Marshal.GetFunctionPointerForDelegate(callback_delegate);

      CAPI.ovr_UnityResetTestPlatform();
      CAPI.ovr_UnityInitWrapperStandalone(accessToken, IntPtr.Zero);
      return true;
    }
  }
}
