using System;
using System.Collections;
using System.Collections.Generic;
using CalcFlowUI;
using Extensions;
using Nanome.Core;
using Nanome.Core.Daemon;
using UnityEngine;

[Serializable]
public partial class PlaybackLogEntry {
    #region other stuff
    private static Dictionary<int, GameObject> objectMap = new Dictionary<int, GameObject> () { { 0, null } };

    public delegate void ReenactAction (LogInfo info, GameObject subject, PlaybackLogEntry entry);
    protected static Dictionary<string, ReenactAction> Reenactors = new Dictionary<string, ReenactAction> ();
    public static void RegisterReenactor (string key, ReenactAction ra) {
        if (!Reenactors.ContainsKey (key)) {
            Reenactors.Add (key, ra);
        } else {
            Debug.Log ("Reenactors already contains key: " + key);
        }
    }

    [SerializeField]
    public int subjectKey;
    [SerializeField]
    public long timeStamp;

    [SerializeField]
    public LogInfo _info = new LogInfo ();

    [HideInInspector]
    [SerializeField]
    internal string binaryRepresentation;

    //how many serializations are left before the scene is done recording.
    public static int numRunningSerializations;
    #endregion
    public static Queue<Action> spawnQueue = new Queue<Action> ();

    public static IEnumerator spawner;
    public static int spawnsPerFrame = 10;

    public static IEnumerator steadySpawn () {
        while (spawnQueue.Count != 0) {
            for (int i = 0; i < spawnsPerFrame; i++) {
                if (spawnQueue.Count == 0) break;
                spawnQueue.Dequeue ().Invoke ();
            }
            yield return null;
        }
        spawner = null;
    }

    // function that will serialize the spawn and assign the binary output to "binaryRepresentation".
    public void SerializeForSpawn (GameObject subject, string key) {
        binaryRepresentation = "";
        binaryRepresentation = RSManager.Serialize (subject, key);
    }

    public void Reenact () {
        GameObject subject;

        string key = _info.GetValue<string> ("key");

        switch (key) {
            case "spawn":
                subject = GetObject (subjectKey);
                ReenactSpawn (_info, subject, this);
                break;
            default:
                subject = GetObject (subjectKey);
                ReenactAction reenactor;
                if (Reenactors.TryGetValue (key, out reenactor)) {
                    reenactor (_info, subject, this);
                } else {
                    Debug.LogError ("Could not find reenactor for key " + key);
                }
                break;
        }
    }
    public GameObject Spawn () {
        GameObject subject;
        //subject = RSManager.DeserializeData<GameObject>(binaryRepresentation, subjectKey.ToString());

        try {
            subject = RSManager.DeserializeData<GameObject> (binaryRepresentation, subjectKey.ToString ());
        } catch (Exception e) {
            Debug.Log ("Exception found in gameobject: " + _info.GetValue<string> ("name") + " with subject key " + subjectKey);
            Debug.LogError (e.Message);
            throw e;
        }

        if (objectMap.ContainsKey (subjectKey)) {
            objectMap[subjectKey] = subject;
        } else {
            objectMap.Add (subjectKey, subject);
        }
        return subject;

    }
    //Basic Reenactors
    #region basic reenactors
    private void ReenactSpawn (LogInfo _info, GameObject subject, PlaybackLogEntry entry) {
        Vector3 position;
        Vector3 scale;
        Quaternion rotation;
        long duration;

        Spawn ();
        subject = objectMap[subjectKey];
        position = _info.GetValue<Vector3> ("position");
        scale = _info.GetValue<Vector3> ("scale");
        rotation = _info.GetValue<Quaternion> ("rotation");

        // if (subject.name == "PieceWiseTabs")
        // {
        //     Debug.Log("delete parent is being made. key: " + subjectKey);
        // }
        subject.MoveTo (position, 0);
        subject.RotateTo (rotation, 0);
        subject.GlobalScaleTo (scale, 0);
    }

    #endregion
    public static GameObject GetObject (int ID) {
        GameObject outObject;
        TryGetObject (ID, out outObject);
        return outObject;
    }

    public static bool TryGetObject (int ID, out GameObject outObject) {
        if (objectMap.ContainsKey (ID)) {
            outObject = objectMap[ID];
            return true;
        } else {
            outObject = null;
            return false;
        }
    }

}