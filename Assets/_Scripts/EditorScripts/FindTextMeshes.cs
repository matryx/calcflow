using System.Collections;
using System.Collections.Generic;
using UnityEngine;
[ExecuteInEditMode]

public class FindTextMeshes : MonoBehaviour
{

    public bool execute = false;

    public HashSet<TextMesh> tmSet = new HashSet<TextMesh>();
    public List<TextMesh> tmList = new List<TextMesh>();
    // Use this for initialization
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        if (!execute) return;


        TextMesh[] allObjects = UnityEngine.Object.FindObjectsOfType<TextMesh>();
        foreach (TextMesh TM in allObjects)
        {
            if (!tmSet.Contains(TM))
            {
                tmSet.Add(TM);
                tmList.Add(TM);
            }

        }
        if (tmSet.Count == 0) print("none found");
        execute = false;
    }
}