using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using Oculus.Avatar;

public class OvrAvatarRemoteDriver : OvrAvatarDriver
{
    Queue<OvrAvatarPacket> packetQueue = new Queue<OvrAvatarPacket>();

    IntPtr CurrentSDKPacket = IntPtr.Zero;
    float CurrentSDKPacketTime = 0f;

    const int MinPacketQueue = 1;
    const int MaxPacketQueue = 4;

    int CurrentSequence = -1;

    public void QueuePacket(int sequence, OvrAvatarPacket packet)
    {
        if (sequence > CurrentSequence)
        {
            CurrentSequence = sequence;
            packetQueue.Enqueue(packet);
        }
    }

    public override void UpdateTransforms(IntPtr sdkAvatar)
    {
        if (CurrentSDKPacket == IntPtr.Zero && packetQueue.Count >= MinPacketQueue)
        {
            CurrentSDKPacket = packetQueue.Dequeue().ovrNativePacket;
        }

        if (CurrentSDKPacket != IntPtr.Zero)
        {
            float PacketDuration = CAPI.ovrAvatarPacket_GetDurationSeconds(CurrentSDKPacket);
            CAPI.ovrAvatar_UpdatePoseFromPacket(sdkAvatar, CurrentSDKPacket, Mathf.Min(PacketDuration, CurrentSDKPacketTime));
            CurrentSDKPacketTime += Time.deltaTime;

            if (CurrentSDKPacketTime > PacketDuration)
            {
                CAPI.ovrAvatarPacket_Free(CurrentSDKPacket);
                CurrentSDKPacket = IntPtr.Zero;
                CurrentSDKPacketTime = CurrentSDKPacketTime - PacketDuration;

                //Throw away packets deemed too old.
                while (packetQueue.Count > MaxPacketQueue)
                {
                    packetQueue.Dequeue();
                }
            }
        }
    }
}