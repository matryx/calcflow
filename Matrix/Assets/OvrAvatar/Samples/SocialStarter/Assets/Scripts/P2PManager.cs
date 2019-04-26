using UnityEngine;
using System;
using Oculus.Platform;
using Oculus.Platform.Models;

// Helper class to manage a Peer-to-Peer connection to the other user.
// The connection is used to send and received the Transforms for the
// Avatars.  The Transforms are sent via unreliable UDP at a fixed
// frequency.
public class P2PManager
{
    // update packet identifier
    private static readonly byte UPDATE_PACKET = 1;
    private static readonly int POSITION_DATA_LENGTH = 41;
    private static readonly float HEIGHT_OFFSET = 0.65f;

    public P2PManager()
    {
        Net.SetPeerConnectRequestCallback(PeerConnectRequestCallback);
        Net.SetConnectionStateChangedCallback(ConnectionStateChangedCallback);
    }

    #region Connection Management

    public void ConnectTo(ulong userID)
    {
        // ID comparison is used to decide who calls Connect and who calls Accept
        if (PlatformManager.MyID < userID)
        {
            Net.Connect(userID);
            PlatformManager.LogOutput("P2P connect to " + userID);
        }
    }

    public void Disconnect(ulong userID)
    {
        if (userID != 0)
        {
            Net.Close(userID);

            RemotePlayer remote = PlatformManager.GetRemoteUser(userID);
            if (remote != null)
            {
                remote.p2pConnectionState = PeerConnectionState.Unknown;
            }
        }
    }

    void PeerConnectRequestCallback(Message<NetworkingPeer> msg)
    {
        PlatformManager.LogOutput("P2P request from " + msg.Data.ID);

        RemotePlayer remote = PlatformManager.GetRemoteUser(msg.Data.ID);
        if (remote != null)
        {
            PlatformManager.LogOutput("P2P request accepted from " + msg.Data.ID);
            Net.Accept(msg.Data.ID);
        }
    }

    void ConnectionStateChangedCallback(Message<NetworkingPeer> msg)
    {
        PlatformManager.LogOutput("P2P state to " + msg.Data.ID + " changed to  " + msg.Data.State);

        RemotePlayer remote = PlatformManager.GetRemoteUser(msg.Data.ID);
        if (remote != null)
        {
            remote.p2pConnectionState = msg.Data.State;

            if (msg.Data.State == PeerConnectionState.Timeout &&
                // ID comparison is used to decide who calls Connect and who calls Accept
                PlatformManager.MyID < msg.Data.ID)
            {
                // keep trying until hangup!
                Net.Connect(msg.Data.ID);
                PlatformManager.LogOutput("P2P re-connect to " + msg.Data.ID);
            }
        }
    }

    #endregion

    #region Send Update

    public void SendAvatarUpdate(ulong userID, Transform bodyTransform, UInt32 sequence, byte[] avatarPacket)
    {
        byte[] sendAvatarBuffer = new byte[avatarPacket.Length + POSITION_DATA_LENGTH];

        sendAvatarBuffer[0] = UPDATE_PACKET;
        int offset = 1;

        PackULong(PlatformManager.MyID, sendAvatarBuffer, ref offset);

        PackFloat(bodyTransform.localPosition.x, sendAvatarBuffer, ref offset);
        PackFloat(bodyTransform.localPosition.y, sendAvatarBuffer, ref offset);
        PackFloat(bodyTransform.localPosition.z, sendAvatarBuffer, ref offset);
        PackFloat(bodyTransform.localRotation.x, sendAvatarBuffer, ref offset);
        PackFloat(bodyTransform.localRotation.y, sendAvatarBuffer, ref offset);
        PackFloat(bodyTransform.localRotation.z, sendAvatarBuffer, ref offset);
        PackFloat(bodyTransform.localRotation.w, sendAvatarBuffer, ref offset);

        PackUInt32(sequence, sendAvatarBuffer, ref offset);

        Buffer.BlockCopy(avatarPacket, 0, sendAvatarBuffer, offset, avatarPacket.Length);
        Net.SendPacket(userID, sendAvatarBuffer, SendPolicy.Unreliable);
    }

    void PackFloat(float f, byte[] buf, ref int offset)
    {
        Buffer.BlockCopy(BitConverter.GetBytes(f), 0, buf, offset, sizeof(float));
        offset = offset + sizeof(float);
    }

    void PackULong(ulong u, byte[] buf, ref int offset)
    {
        Buffer.BlockCopy(BitConverter.GetBytes(u), 0, buf, offset, sizeof(ulong));
        offset = offset + sizeof(ulong);
    }

    void PackUInt32(UInt32 u, byte[] buf, ref int offset)
    {
        Buffer.BlockCopy(BitConverter.GetBytes(u), 0, buf, offset, sizeof(UInt32));
        offset = offset + sizeof(UInt32);
    }

    #endregion

    #region Receive Update

    public void GetRemotePackets()
    {
        Packet packet;

        while ((packet = Net.ReadPacket()) != null)
        {
            byte[] receiveAvatarBuffer = new byte[packet.Size];
            packet.ReadBytes(receiveAvatarBuffer);

            if (receiveAvatarBuffer[0] != UPDATE_PACKET)
            {
                PlatformManager.LogOutput("Invalid packet type: " + packet.Size);
                continue;
            }
            processAvatarPacket(ref receiveAvatarBuffer);
        }
    }

    public void processAvatarPacket(ref byte[] packet)
    {
        ulong remoteUserID = 0;

        remoteUserID = BitConverter.ToUInt64(packet, 1);

        RemotePlayer remote = PlatformManager.GetRemoteUser(remoteUserID);
        if (remote != null)
        {
            remote.receivedBodyPositionPrior = remote.receivedBodyPosition;
            remote.receivedBodyPosition.x = BitConverter.ToSingle(packet, 9);
            remote.receivedBodyPosition.y = BitConverter.ToSingle(packet, 13) + HEIGHT_OFFSET;
            remote.receivedBodyPosition.z = BitConverter.ToSingle(packet, 17);

            remote.receivedBodyRotationPrior = remote.receivedBodyRotation;
            remote.receivedBodyRotation.x = BitConverter.ToSingle(packet, 21);
            remote.receivedBodyRotation.y = BitConverter.ToSingle (packet, 25);
            remote.receivedBodyRotation.z = BitConverter.ToSingle(packet, 29);
            remote.receivedBodyRotation.w = BitConverter.ToSingle (packet, 33);

            int sequence = BitConverter.ToInt32(packet, 37);

            byte[] receiveAvatarBuffer = new byte[packet.Length - POSITION_DATA_LENGTH];
            Buffer.BlockCopy(packet, POSITION_DATA_LENGTH, receiveAvatarBuffer, 0, receiveAvatarBuffer.Length);

            IntPtr avatarPacket = Oculus.Avatar.CAPI.ovrAvatarPacket_Read((UInt32)receiveAvatarBuffer.Length, receiveAvatarBuffer);

            remote.RemoteAvatar.GetComponent<OvrAvatarRemoteDriver>().QueuePacket(sequence, new OvrAvatarPacket { ovrNativePacket = avatarPacket });

            remote.RemoteAvatar.transform.localPosition = remote.receivedBodyPosition;
            remote.RemoteAvatar.transform.localRotation = remote.receivedBodyRotation;
        }
    }

    #endregion
}
