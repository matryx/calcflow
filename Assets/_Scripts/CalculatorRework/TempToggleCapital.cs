using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TempToggleCapital : QuickButton {
    Transform letterPanel;
    bool capitalized = false;

	void Awake () {
        letterPanel = transform.parent.parent.parent.Find("LetterPanel");
    }

    protected override void Start()
    {
        base.Start();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        toggleCapital();
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    private void toggleCapital()
    {
        foreach (Transform child in letterPanel)
        {
            if (child.name != "ToggleCaps")
            {
                child.name = (capitalized) ? child.name.ToLower() : child.name.ToUpper();

                child.GetComponentInChildren<TextMesh>().text = (capitalized) ?
                                                                 child.GetComponentInChildren<TextMesh>().text.ToLower() :
                                                                 child.GetComponentInChildren<TextMesh>().text.ToUpper();
            }
            else
            {
                child.GetComponentInChildren<TextMesh>().text = (capitalized) ?
                                                                 child.GetComponentInChildren<TextMesh>().text.ToUpper() :
                                                                 child.GetComponentInChildren<TextMesh>().text.ToLower();
            }
        }

        capitalized = !capitalized;
    }

    void Update () {
		
	}
}
