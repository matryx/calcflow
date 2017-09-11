using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Oculus.Platform;

public class RemotePlayer {
    public ulong remoteUserID;
    public bool stillInRoom;

    // the result of the last connection state update message
    public PeerConnectionState p2pConnectionState;
    // the last reported state of the VOIP connection
    public PeerConnectionState voipConnectionState;

    public OvrAvatar RemoteAvatar;
    // The in-scene objects representing the user
    public GameObject remotePlayerBody;

    // the last received position updates
    public Vector3 receivedBodyPosition;

    // the previous received positions to interpolate from
    public Vector3 receivedBodyPositionPrior;

    // the last received rotation updates
    public Quaternion receivedBodyRotation;

    // the previous received rotations to interpolate from
    public Quaternion receivedBodyRotationPrior;
}
