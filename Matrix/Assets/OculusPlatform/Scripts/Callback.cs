namespace Oculus.Platform
{
  using UnityEngine;
  using System;
  using System.Collections.Generic;

  public static class Callback
  {
    #region Notification Callbacks: Exposed through Oculus.Platform.Platform

    internal static void SetNotificationCallback<T>(Message.MessageType type, Message<T>.Callback callback)
    {
      if (callback == null) {
        throw new Exception ("Cannot provide a null notification callback.");
      }

      notificationCallbacks[type] = new RequestCallback<T>(callback);

      if (type == Message.MessageType.Notification_Room_InviteAccepted)
      {
          FlushRoomInviteNotificationQueue();
      }
    }

    internal static void SetNotificationCallback(Message.MessageType type, Message.Callback callback)
    {
      if (callback == null) {
        throw new Exception ("Cannot provide a null notification callback.");
      }

      notificationCallbacks[type] = new RequestCallback(callback);
    }
    #endregion 

    #region OnComplete Callbacks: Exposed through Oculus.Platform.Request
    internal static void OnComplete<T>(Request<T> request, Message<T>.Callback callback)
    {
      requestIDsToCallbacks[request.RequestID] = new RequestCallback<T>(callback);
    }
    internal static void OnComplete(Request request, Message.Callback callback)
    {
      requestIDsToCallbacks[request.RequestID] = new RequestCallback(callback);
    }

    internal static void RunCallbacks()
    {
      while (true)
      {
        var msg = Platform.Message.PopMessage();
        if (msg == null)
        {
          break;
        }

        HandleMessage(msg);
      }

    }

    internal static void RunLimitedCallbacks(uint limit)
    {
      for (var i = 0; i < limit; ++i)
      {
        var msg = Platform.Message.PopMessage();
        if (msg == null)
        {
          break;
        }

        HandleMessage(msg);
      }
    }
    #endregion

    #region Callback Internals
    private static Dictionary<ulong, RequestCallback> requestIDsToCallbacks = new Dictionary<ulong, RequestCallback>();
    private static Dictionary<Message.MessageType, RequestCallback> notificationCallbacks = new Dictionary<Message.MessageType, RequestCallback>();

    private static bool hasRegisteredRoomInviteNotificationHandler = false;
    private static List<Message> pendingRoomInviteNotifications = new List<Message>();
    private static void FlushRoomInviteNotificationQueue() {
        hasRegisteredRoomInviteNotificationHandler = true;
        foreach (Message msg in pendingRoomInviteNotifications) {
            HandleMessage(msg);
        }
        pendingRoomInviteNotifications.Clear();
    }

    private class RequestCallback
    {
      private Message.Callback messageCallback;

      public RequestCallback() { }

      public RequestCallback(Message.Callback callback)
      {
        this.messageCallback = callback;
      }

      public virtual void HandleMessage(Message msg)
      {
        if (messageCallback != null)
        {
          messageCallback(msg);
        }
      }
    }

    private sealed class RequestCallback<T> : RequestCallback
    {
      private Message<T>.Callback callback;
      public RequestCallback(Message<T>.Callback callback)
      {
        this.callback = callback;
      }

      public override void HandleMessage(Message msg)
      {
        if (callback != null)
        {

            // We need to queue up GameInvites because the callback runner will be called before a handler has beeen set.
            if (!hasRegisteredRoomInviteNotificationHandler && msg.Type == Message.MessageType.Notification_Room_InviteAccepted)
            {
                pendingRoomInviteNotifications.Add(msg);
                return;
            }

          if (msg is Message<T>)
          {
            callback((Message<T>)msg);
          }
          else
          {
            Debug.LogError("Unable to handle message: " + msg.GetType());
          }
        }
      }
    }

    private static void HandleMessage(Message msg)
    {
      RequestCallback callbackHolder;
      if (requestIDsToCallbacks.TryGetValue(msg.RequestID, out callbackHolder))
      {
        try
        {
          callbackHolder.HandleMessage(msg);
        }
        // even if there are exceptions, we should clean up cleanly
        finally
        {
          requestIDsToCallbacks.Remove(msg.RequestID);
        }
      }
      else if (notificationCallbacks.TryGetValue(msg.Type, out callbackHolder))
      {
        callbackHolder.HandleMessage(msg);
      }
    }

    #endregion
  }
}
