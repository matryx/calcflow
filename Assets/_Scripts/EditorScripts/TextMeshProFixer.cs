using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
[ExecuteInEditMode]



public class TextMeshProFixer : MonoBehaviour {

    public bool execute = false;
    public TMPro.TMP_FontAsset Arial;	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        if (!execute) return;

        TMPro.TextMeshPro[] allObjects = (TextMeshPro[])Resources.FindObjectsOfTypeAll(typeof(TextMeshPro));
		allObjects = UnityEngine.Object.FindObjectsOfType<TextMeshPro>();
        foreach (TextMeshPro TMP in allObjects)
        {
            TMP.font = Arial;
        }
		execute = false;
    }
}
