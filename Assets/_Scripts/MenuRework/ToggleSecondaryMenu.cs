using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleSecondaryMenu : QuickButton
{
    Transform secondaryMenu;
    FuseButton fuseButton;
    float zRotation;

    // Use this for initialization
    void Awake()
    {
        GameObject secondary = GameObject.Find("/SecondaryMenus");
        if (secondary != null)
        {
            secondaryMenu = secondary.transform;
        }
        else
        {
            Destroy(this.gameObject);
            return;
        }
        //setMenuPos();
        //zRotation = secondaryMenu.localEulerAngles.z;
        secondaryMenu.gameObject.SetActive(false);

        fuseButton = this.transform.parent.GetComponentInChildren<FuseButton>();

    }

    // Update is called once per frame
    void Update()
    {

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
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        fuseButton.ForceCold();
    }
}
