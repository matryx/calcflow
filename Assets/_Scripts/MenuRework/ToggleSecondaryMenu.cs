using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleSecondaryMenu : QuickButton {
    public Transform secondaryMenu;
    public Transform view;
    public Transform circle;
    float zRotation;

	// Use this for initialization
	void Awake () {
        setMenuPos();
        zRotation = secondaryMenu.localEulerAngles.z;
        secondaryMenu.gameObject.SetActive(false);
	}
	
    void setMenuPos()
    {
        secondaryMenu.SetParent(view);
        secondaryMenu.localPosition = new Vector3(0, 0, 0.7f);
        secondaryMenu.localEulerAngles = new Vector3(0, 0, 0);
        secondaryMenu.SetParent(null);
        secondaryMenu.localEulerAngles = new Vector3(secondaryMenu.localEulerAngles.x,
            secondaryMenu.localEulerAngles.y, zRotation);
        secondaryMenu.localScale = Vector3.one;
        circle.localScale = new Vector3(0.5f, 0.01f, 0.5f);
    }

	// Update is called once per frame
	void Update () {
		
	}

    protected override void Start()
    {
        base.Start();

    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (secondaryMenu.gameObject.activeSelf)
        {
            secondaryMenu.gameObject.SetActive(false);
        }
        else
        {
            secondaryMenu.gameObject.SetActive(true);
            setMenuPos();
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        
    }
}
