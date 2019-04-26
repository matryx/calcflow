using UnityEngine;
using System;
using System.Collections.Generic;
using Oculus.Platform;
using Oculus.Platform.Models;

// Helper class to manage Room creation, membership and invites.
// Rooms are a mechanism to help Oculus users create a shared experience.
// Users can only be in one Room at a time.  If the Owner of a room
// leaves, then ownership is transferred to some other member.
// Here we use rooms to create the notion of a 'call' to help us
// invite a Friend and establish a VOIP and P2P connection.
public class RoomManager
{
    // the ID of the Room that I'm in
    public ulong roomID;

    // the ID of the Room that I'm invited to
    private ulong invitedRoomID;   

    // Am I the server?
    private bool amIServer;

    // Have we already gone through the startup?
    private bool startupDone;

    public RoomManager()
    {
        amIServer = false;
        startupDone = false;
        Rooms.SetRoomInviteNotificationCallback(AcceptingInviteCallback);
        Rooms.SetUpdateNotificationCallback(RoomUpdateCallback);
    }

    #region Launched Application from Accepting Invite

    // Callback to check whether the User accepted an invite
    void AcceptingInviteCallback(Message<string> msg)
    {
        if (msg.IsError)
        {
            PlatformManager.TerminateWithError(msg);
            return;
        }

        PlatformManager.LogOutput("Launched Invite to join Room: " + msg.Data);

        invitedRoomID = Convert.ToUInt64(msg.GetString());

        if (startupDone)
        {
            CheckForInvite();
        }
    }

    // Check to see if the App was launched by accepting the Notication from the main Oculus app.
    // If so, we can directly join that room.  (If it's still available.)
    public bool CheckForInvite()
    {
        startupDone = true;

        if (invitedRoomID != 0)
        {
            JoinExistingRoom(invitedRoomID);
            return true;
        }
        else
        {
            return false;
        }
    }

    #endregion

    #region Create a Room and Invite Friend(s) from the Oculus Universal Menu

    public void CreateRoom()
    {
        Rooms.CreateAndJoinPrivate(RoomJoinPolicy.InvitedUsers, 4, true)
             .OnComplete(CreateAndJoinPrivateRoomCallback);
    }

    void CreateAndJoinPrivateRoomCallback(Message<Oculus.Platform.Models.Room> msg)
    {
        if (msg.IsError)
        {
            PlatformManager.TerminateWithError(msg);
            return;
        }

        roomID = msg.Data.ID;

        if (msg.Data.Owner.ID == PlatformManager.MyID)
        {
            amIServer = true;
        }
        else
        {
            amIServer = false;
        }

        PlatformManager.TransitionToState(PlatformManager.State.WAITING_IN_A_ROOM);
        PlatformManager.SetFloorColorForState(amIServer);
    }

    void OnLaunchInviteWorkflowComplete(Message msg)
    {
        if (msg.IsError)
        {
            PlatformManager.TerminateWithError(msg);
            return;
        }
    }

    #endregion

    #region Accept Invite

    public void JoinExistingRoom(ulong roomID)
    {
        PlatformManager.TransitionToState(PlatformManager.State.JOINING_A_ROOM);
        Rooms.Join(roomID, true).OnComplete(JoinRoomCallback);
    }

    void JoinRoomCallback(Message<Oculus.Platform.Models.Room> msg)
    {
        if (msg.IsError)
        {
            // is reasonable if caller called more than 1 person, and I didn't answer first
            return;
        }

        PlatformManager.LogOutput("Joined Room " + msg.Data.ID + " owner: " + msg.Data.Owner.OculusID + " count: " + msg.Data.Users.Count);
        roomID = msg.Data.ID;
        ProcessRoomData (msg);
    }

    #endregion

    #region Room Updates

    void RoomUpdateCallback(Message<Oculus.Platform.Models.Room> msg)
    {
        if (msg.IsError)
        {
            PlatformManager.TerminateWithError(msg);
            return;
        }

        PlatformManager.LogOutput("Room Update " + msg.Data.ID + " owner: " + msg.Data.Owner.OculusID + " count: " + msg.Data.Users.Count);
        ProcessRoomData (msg);
    }

    #endregion

    #region Room Exit

    public void LeaveCurrentRoom()
    {
        if (roomID != 0)
        {
            Rooms.Leave(roomID);
            roomID = 0;
        }
        PlatformManager.TransitionToState(PlatformManager.State.LEAVING_A_ROOM);
    }

    #endregion

    #region Process Room Data

    void ProcessRoomData(Message<Oculus.Platform.Models.Room> msg)
    {
        if (msg.Data.Owner.ID == PlatformManager.MyID)
        {
            amIServer = true;
        }
        else
        {
            amIServer = false;
        }

        // if the caller left while I was in the process of joining, just use that as our new room
        if (msg.Data.Users.Count == 1)
        {
            PlatformManager.TransitionToState(PlatformManager.State.WAITING_IN_A_ROOM);
        }
        else
        {
            PlatformManager.TransitionToState(PlatformManager.State.CONNECTED_IN_A_ROOM);
        }

        // Look for users that left
        PlatformManager.MarkAllRemoteUsersAsNotInRoom();

        foreach (User user in msg.Data.Users)
        {
            if (user.ID != PlatformManager.MyID)
            {
                if (!PlatformManager.IsUserInRoom (user.ID)) 
                {
                    PlatformManager.AddRemoteUser (user.ID);
                } 
                else 
                {
                    PlatformManager.MarkRemoteUserInRoom (user.ID);
                }
            }
        }

        PlatformManager.ForgetRemoteUsersNotInRoom();
        PlatformManager.SetFloorColorForState(amIServer);	
    }

    #endregion
}
