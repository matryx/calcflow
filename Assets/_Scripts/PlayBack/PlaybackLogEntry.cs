using System;
using System.Collections;
using System.Collections.Generic;
using CalcFlowUI;
using Extensions;
using Nanome.Core;
using Nanome.Core.Daemon;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[Serializable]
public partial class PlaybackLogEntry
{
    #region other stuff
    private static Dictionary<string, GameObject> objectMap = new Dictionary<string, GameObject>() { { "", null } };

    public delegate void ReenactAction(LogInfo info, GameObject subject, PlaybackLogEntry entry);
    protected static Dictionary<string, ReenactAction> Reenactors = new Dictionary<string, ReenactAction>();
    public static void RegisterReenactor(string key, ReenactAction ra)
    {
        if (!Reenactors.ContainsKey(key))
        {
            Reenactors.Add(key, ra);
        }
        else
        {
            Debug.Log("Reenactors already contains key: " + key);
        }
    }

    [SerializeField]
    public string subjectKey;
    [SerializeField]
    public long timeStamp;
    [SerializeField]
    public string name;

    [SerializeField]
    public LogInfo _info = new LogInfo();

    [HideInInspector]
    [SerializeField]
    internal string binaryRepresentation;

    //how many serializations are left before the scene is done recording.
    public static int numRunningSerializations;
    #endregion

    public static string GetUniqueID(GameObject subject)
    {
        UIDSystem uid = subject.GetComponent<UIDSystem>();
        if (uid == null)
        {
            Debug.LogError("subject " + subject.name + " does not have a uidSystem");
        }

        return uid.GetGameObjectIdentifier();
    }

    // function that will serialize the spawn and assign the binary output to "binaryRepresentation".
    public void SerializeForSpawn(GameObject subject, string key)
    {
        binaryRepresentation = "";
        binaryRepresentation = RSManager.Serialize(subject, key);
    }

    public void Reenact()
    {
        GameObject subject;
        subject = GetObject(subjectKey);


        string key = _info.GetValue<string>("key");

        switch (key)
        {
            case "spawn":
                ReenactSpawn(_info, subject, this);
                break;
            default:
                ReenactAction reenactor;

                if (Reenactors.TryGetValue(key, out reenactor))
                {
                    reenactor(_info, subject, this);
                }
                else
                {
                    Debug.LogError("Could not find reenactor for key " + key);
                }
                break;
        }
    }
    public GameObject Spawn()
    {
        GameObject subject;

        try
        {
            subject = RSManager.DeserializeData<GameObject>(binaryRepresentation, subjectKey);
        }
        catch (Exception e)
        {
            Debug.Log("Exception found in gameobject: " + _info.GetValue<string>("name") + " with subject key " + subjectKey);
            Debug.LogError(e.Message);
            throw e;
        }
        RegisterObject(subject);

        return subject;

    }

    private void RegisterObject(GameObject subject)
    {
        string uniqueID = GetUniqueID(subject);
        if (objectMap.ContainsKey(uniqueID))
        {
            objectMap[uniqueID] = subject;
        }
        else
        {
            objectMap.Add(uniqueID, subject);
        }

        foreach (Transform child in subject.transform)
        {
            RegisterObject(child.gameObject);
        }
    }
    //Basic Reenactors
    private void ReenactSpawn(LogInfo _info, GameObject subject, PlaybackLogEntry entry)
    {
        UnityEngine.Debug.Log("<color=green>replaying spawn " + subjectKey + "</color>");
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;

        subject = Spawn();
        position = _info.GetValue<Vector3>("position");
        scale = _info.GetValue<Vector3>("scale");
        rotation = _info.GetValue<Quaternion>("rotation");

        // if (subject.name == "PieceWiseTabs")
        // {
        //     Debug.Log("delete parent is being made. key: " + subjectKey);
        // }
        subject.MoveTo(position, 0);
        subject.RotateTo(rotation, 0);
        subject.GlobalScaleTo(scale, 0);
    }

    public static GameObject GetObject(string ID)
    {
        GameObject outObject;
        TryGetObject(ID, out outObject);
        return outObject;
    }

    public static bool TryGetObject(string ID, out GameObject outObject)
    {
        if (objectMap.ContainsKey(ID))
        {
            outObject = objectMap[ID];
            return true;
        }
        else
        {
            outObject = null;
            return false;
        }
    }

}