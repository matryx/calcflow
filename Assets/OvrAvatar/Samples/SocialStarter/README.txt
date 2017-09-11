The SocialStarter sample is meant to show how to leverage the Oculus Avatars and Oculus platform features together to make a very basic networked social experience.

Steps to run the sample:

1. Import the Oculus Unity Utilities Package if you don't already have it included in your project.  This will provide you with all of the necessary general Oculus Prefabs.
2. Import the Oculus Avatar SDK Unity Package if you don't already have it included in your project.  This will provide you with all of the necessary Avatar Prefabs.
3. Import the Oculus Platform SDK Unity Package if you don't already have it included in your project.  This will provide all of the C# interfaces to the Oculus Platform features.
4. In the Unity editor menus select "Oculus Avatars > Edit Configuration". Place your AppID in the two fields for Rift AppID and Gear AppID.
5. In the Unity editor menus select "Oculus Platform > Edit Settings". Place your AppID in the two fields for Rift AppID and Gear AppID.
6. Make sure the prefabs are set correctly. Select the OVRPlayerController object in the MainScene:
a) Local Avatar Prefab should be set to the "LocalAvatar (OvrAvatar)" prefab found in the Avatar SDK under "OvrAvatar > Content > Prefabs".
b) Remote Avatar Prefab should be set to the "RemoteAvatar (OvrAvatar)" prefab found in the Avatar SDK under "OvrAvatar > Content > Prefabs".

How to use

1. When you first start up the sample you are placed in both a virtual room and an online room.  In the virtual room the colour of both the floor and the sphere in the middle are used as state indicators:

a) The sphere colour indicates if you are successfully in an online room.  Black means the room creation/join failed for some reason, White means you are in an online room.
b) The floor colour is blue if you are the owner of the room or green if you are just a member of the room.

2. Your left hand should be holding the instructions UI:

Button X: Send an Invite - This will bring up the Oculus Universal Menu which will show a list of your friends that you can invite. This may take a second or two to pop up.
Button Y: Toggle the sky camera - This allows you to view the scene from a static 3rd person camera.
Click left Stick: Toggle showing the instructions UI
Left Stick Analog: Move around the space
Right Stick Analog: Rotate

3. When a user joins your room a VoIP connection and a P2P connection will be setup. The P2P connection is used to send Avatar updates as well as positional updates.
