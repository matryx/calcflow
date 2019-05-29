using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class QuickButton : MenuStateReceiver {

    public static Color TOGGLE_OFF = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    public static Color TOGGLE_ON = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    public static Color LIGHT_PASSIVE = Color.white;
    public static Color DARK_PASSIVE = new Color(0.2f, 0.475f, 0.565f);
    public static Color LIGHT_HOVERING = new Color(132f / 255f, 223f / 255f, 253f / 255f);
    public static Color DARK_HOVERING = new Color(87f / 255f, 178f / 255f, 208f / 255f);

    protected virtual void Start()
    {
        VirtualButton virtualButton = GetComponent<VirtualButton>();
        if (virtualButton != null)
        {
            virtualButton.OnButtonPress += ButtonEnterBehavior;
            virtualButton.OnButtonUnpress += ButtonExitBehavior;
        }

        RayCastButton rcButton = GetComponent<RayCastButton>();
        if (rcButton != null)
        {
            rcButton.OnButtonStay += ButtonStayBehavior;
            rcButton.OnButtonPress += ButtonEnterBehavior;
            rcButton.OnButtonUnpress += ButtonExitBehavior;
            rcButton.OnButtonLeave += ButtonLeaveBehavior;
        }

        TouchButton touchButton = GetComponent<TouchButton>();
        if (touchButton != null)
        {
            touchButton.OnButtonPress += ButtonEnterBehavior;
            touchButton.OnButtonUnpress += ButtonExitBehavior;
        }
    }

    protected virtual void ButtonStayBehavior(GameObject other) { }

    protected abstract void ButtonEnterBehavior(GameObject other);

    protected abstract void ButtonExitBehavior(GameObject other);

    protected virtual void ButtonLeaveBehavior(GameObject other) { }

    public override void OnMenuOpen() { }
    public override void OnMenuClose() { }
    public override void OnMenuClosed() { }
    public override void OnMenuLoad() { }
    public override void OnMenuUnload() { }
}
