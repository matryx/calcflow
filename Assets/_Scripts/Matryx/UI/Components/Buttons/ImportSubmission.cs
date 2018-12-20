using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

using Matryx;

public class ImportSubmission : QuickButton {

    public MatryxSubmission submission;

    [SerializeField]
    private CalcManager calcManager;
    [SerializeField]
    private FlexButtonComponent submissionButtonFlexComponent;

    protected override void ButtonEnterBehavior(GameObject other)
    {
        // Select the button
        submissionButtonFlexComponent.SetState(2);

        var equationJson = JsonHelper.FromJson<SerializableExpressionSet>(submission.EquationJson);
        List<ExpressionSet> ess = equationJson.Select(x => x.ConvertToExpressionSet()).ToList();
        calcManager.LoadSavedExpressionSets(ess);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        submissionButtonFlexComponent.SetState(1);
    }

    public void Disable()
    {
        submissionButtonFlexComponent.SetState(0);
    }

    public void Reenable()
    {
        submissionButtonFlexComponent.SetState(1);
    }
}
