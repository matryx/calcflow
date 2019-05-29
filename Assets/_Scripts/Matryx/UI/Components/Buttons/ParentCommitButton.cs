using Matryx;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParentCommitButton : QuickButton {
    [SerializeField]
    public TMPro.TextMeshPro parentCommitLabel;
    public MatryxCommit commit;

    private Color TOGGLE_ON = new Color(83f / 255f, 198f / 255f, 236f / 255f);
    private Color TOGGLE_OFF = new Color(117f / 255f, 205f / 255f, 234f / 255f);
    private Color DARK_PASSIVE = new Color(0.2f, 0.475f, 0.565f);
    private Color LIGHT_PASSIVE = Color.white;
    private Color DARK_HOVERING = new Color(87f / 255f, 178f / 255f, 208f / 255f);
    private Color LIGHT_HOVERING = new Color(132f / 255f, 223f / 255f, 253f / 255f);

    private float defaultFontSize = 1.4f;
    private float otherFontSize = 1.3f;

    protected override void ButtonEnterBehavior(GameObject other)
    {
        CommitMenu.Instance.SetCommit(commit.parentHash);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
       
    }
}
