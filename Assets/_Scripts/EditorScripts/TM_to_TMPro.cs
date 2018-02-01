using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Extensions;
using TMPro;
[ExecuteInEditMode]


public class TM_to_TMPro : MonoBehaviour
{

    public bool execute = false;

    // Update is called once per frame
    void Update()
    {
        if (!execute) return;
        execute = false;

        TextMesh[] allObjects = (TextMesh[])Resources.FindObjectsOfTypeAll(typeof(TextMesh));

        foreach (TextMesh TM in allObjects)
        {
            GameObject gobj = TM.gameObject;
            string text = TM.text;
            Color color = TM.color;
            gobj.EnsureNoneOf<TextMesh>();
            TextMeshPro TMP = gobj.EnsureOneOf<TextMeshPro>();
            TMP.text = text;
            TMP.color = color;
			RectTransform trans = TMP.gameObject.GetComponent<RectTransform>();
			trans.localScale = new Vector3(.05f, .05f, .05f);
        }
    }
}
