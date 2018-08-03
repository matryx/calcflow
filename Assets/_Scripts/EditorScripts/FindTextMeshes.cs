using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using VoxelBusters.RuntimeSerialization;
[ExecuteInEditMode]

public class FindTextMeshes : MonoBehaviour
{

    public bool execute = false;
    public bool replace = false;

    public HashSet<TextMesh> tmSet = new HashSet<TextMesh>();
    public List<TextMesh> tmList = new List<TextMesh>();
    // Use this for initialization
    void Start()
    {

    }

    // Update is called once per frame
    void Update()
    {
        if (execute)
        {
            execute = false;
            tmSet = new HashSet<TextMesh>();
            tmList = new List<TextMesh>();

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
        }
        if (replace)
        {
            replace = false;
            ReplaceTM();
        }
    }

    void ReplaceTM()
    {
        foreach (TextMesh tm in tmList)
        {

            string name = tm.gameObject.name;
            GameObject go = RSUtility.CreateGameObject("TESTINGTESTING");
            go.transform.parent = tm.transform.parent;
            go.transform.localPosition = tm.transform.localPosition + new Vector3 (0,0,tm.offsetZ);
            go.transform.localRotation = tm.transform.localRotation;
            go.transform.localScale = tm.transform.localScale;
            TextMeshPro tmp = go.AddComponent<TextMeshPro>();

            tmp.text = tm.text;
            tmp.transform.localScale = tmp.transform.localScale * 10 * tm.characterSize;
            TextAlignment alignment = tm.alignment;
            switch (alignment)
            {
                case (TextAlignment.Center):
                    tmp.alignment = TextAlignmentOptions.Center;
                    break;
                case (TextAlignment.Right):
                    tmp.alignment = TextAlignmentOptions.Right;
                    break;
                case (TextAlignment.Left):
                    tmp.alignment = TextAlignmentOptions.Left;
                    break;
            }
            tmp.enableWordWrapping = false;
            TextAnchor anchor = tm.anchor;
            switch (anchor)
            {
                case (TextAnchor.LowerLeft):
                    tmp.rectTransform.pivot = new Vector2(0, 1);
                    break;
                case (TextAnchor.MiddleLeft):
                    tmp.rectTransform.pivot = new Vector2(0, .5f);
                    break;
                case (TextAnchor.UpperLeft):
                    tmp.rectTransform.pivot = new Vector2(0, 0);
                    break;
                case (TextAnchor.LowerCenter):
                    tmp.rectTransform.pivot = new Vector2(.5f, 1);
                    break;
                case (TextAnchor.MiddleCenter):
                    tmp.rectTransform.pivot = new Vector2(.5f, .5f);
                    break;
                case (TextAnchor.UpperCenter):
                    tmp.rectTransform.pivot = new Vector2(.5f, 0);
                    break;
                case (TextAnchor.LowerRight):
                    tmp.rectTransform.pivot = new Vector2(1, 1);
                    break;
                case (TextAnchor.MiddleRight):
                    tmp.rectTransform.pivot = new Vector2(1, .5f);
                    break;
                case (TextAnchor.UpperRight):
                    tmp.rectTransform.pivot = new Vector2(1, 0);
                    break;
            }
            tmp.color = tm.color;
        }
    }
}
