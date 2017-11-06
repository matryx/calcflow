using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TextLogger : MonoBehaviour {

    public string text = "Hello World";
    public float offsetZ = 0;
    public float characterSize = 1;
    public float lineSpacing = 1;
    public TextAnchor anchor = TextAnchor.UpperLeft;
    public TextAlignment alignment = TextAlignment.Left;
    public float tabSize = 4;
    public int fontSize = 0;
    public FontStyle fontStyle = FontStyle.Normal;
    public bool richText = true;
    public Font font;
    public Color color = Color.white;

    // Use this for initialization
    void Start () {
        TextMesh tm = gameObject.AddComponent<TextMesh>();
        tm.text = text;
        tm.offsetZ = offsetZ;
        tm.characterSize = characterSize;
        tm.lineSpacing = lineSpacing;
        tm.anchor = anchor;
        tm.alignment = alignment;
        tm.tabSize = tabSize;
        tm.fontSize = fontSize;
        tm.fontStyle = fontStyle;
        tm.richText = richText;
        tm.font = font;
        tm.color = color;
    }
	
	// Update is called once per frame
	void Update () {
		
	}
}
