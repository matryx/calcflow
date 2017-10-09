using System.Collections;
using System.Collections.Generic;
using UnityEngine;
[ExecuteInEditMode]

public class TextMeshFixer : MonoBehaviour {

    public bool execute = false;
    public Font Quicksand;
    public Material fontMaterial;
	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        if (!execute) return;


        TextMesh[] allObjects = UnityEngine.Object.FindObjectsOfType<TextMesh>();
        foreach (TextMesh TM in allObjects)
        {
            TM.gameObject.GetComponent<MeshRenderer>().material = fontMaterial;
            TM.offsetZ = -.06f;
            TM.font = Quicksand;

        }
        DestroyImmediate(this);

    }
}
